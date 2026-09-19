#define _GNU_SOURCE
#include "session-ipc.h"
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif

extern char **environ;
static int wake_pipe[2];
static volatile sig_atomic_t stopping;
static struct { pid_t pid, group; int monitored; } children[256];

static void notify(int number)
{
    int saved = errno;
    char byte = 0;
    if (number != SIGCHLD) stopping = number;
    (void)write(wake_pipe[1], &byte, 1);
    errno = saved;
}

static void close_after_three(void)
{
#if defined(__linux__) && defined(SYS_close_range)
    if (syscall(SYS_close_range, 4U, ~0U, 0) == 0) return;
#endif
#ifdef F_CLOSEM
    if (fcntl(4, F_CLOSEM, 0) == 0) return;
#endif
    long maximum = sysconf(_SC_OPEN_MAX), descriptor;
    for (descriptor = 4; descriptor < maximum; descriptor++) close((int)descriptor);
}

static char *next_string(char **cursor, char *end)
{
    char *start = *cursor, *nul = memchr(start, 0, (size_t)(end - start));
    if (!nul) return NULL;
    *cursor = nul + 1;
    return start;
}

static int spawn_child(char *body, uint32_t length, int *fds, uint32_t count)
{
    uint32_t fields[3], index;
    int slot, channel[2], failure = 0;
    char *cursor = body + sizeof(fields), *end = body + length;
    char *cwd, *program, **arguments = NULL, **environment = NULL;
    pid_t pid;
    ssize_t received;
    if (length < sizeof(fields) || count != 3) return -EPROTO;
    memcpy(fields, body, sizeof(fields));
    if (fields[1] > 16384 || fields[2] > 16384) return -E2BIG;
    for (slot = 0; slot < 256 && children[slot].pid; slot++) {}
    if (slot == 256) return -EAGAIN;
    cwd = next_string(&cursor, end); program = next_string(&cursor, end);
    if (!cwd || !program) return -EPROTO;
    arguments = calloc(fields[1] + 2, sizeof(char *));
    environment = calloc(fields[2] + 1, sizeof(char *));
    if (!arguments || !environment) { failure = ENOMEM; goto done; }
    arguments[0] = program;
    for (index = 0; index < fields[1]; index++)
        if (!(arguments[index + 1] = next_string(&cursor, end))) {
            failure = EPROTO; goto done;
        }
    for (index = 0; index < fields[2]; index++)
        if (!(environment[index] = next_string(&cursor, end))) {
            failure = EPROTO; goto done;
        }
    if (cursor != end) { failure = EPROTO; goto done; }
    if (pipe(channel) < 0) { failure = errno; goto done; }
    fcntl(channel[0], F_SETFD, FD_CLOEXEC);
    fcntl(channel[1], F_SETFD, FD_CLOEXEC);
    pid = fork();
    if (pid == 0) {
        struct sigaction action = {0};
        sigset_t empty;
        int signal_number;
        close(channel[0]);
        if (setpgid(0, (pid_t)fields[0]) < 0) {
            failure = errno;
            dprintf(2, "cclsh-session: child %ld cannot join group %u (session %ld): %s\n",
                    (long)getpid(), fields[0], (long)getsid(0), strerror(failure));
            errno = failure;
            goto child_error;
        }
        if (chdir(cwd) < 0 ||
            dup2(fds[0], 0) < 0 || dup2(fds[1], 1) < 0 || dup2(fds[2], 2) < 0)
            goto child_error;
        if (dup2(channel[1], 3) < 0) goto child_error;
        channel[1] = 3;
        fcntl(3, F_SETFD, FD_CLOEXEC);
        close_after_three();
        action.sa_handler = SIG_DFL;
        sigemptyset(&action.sa_mask);
        for (signal_number = 1; signal_number < NSIG; signal_number++)
            if (signal_number != SIGKILL && signal_number != SIGSTOP)
                sigaction(signal_number, &action, NULL);
        sigemptyset(&empty);
        sigprocmask(SIG_SETMASK, &empty, NULL);
        execve(program, arguments, environment);
child_error:
        failure = errno;
        (void)write(channel[1], &failure, sizeof(failure));
        _exit(127);
    }
    close(channel[1]);
    if (pid < 0) { failure = errno; close(channel[0]); goto done; }
    do { received = read(channel[0], &failure, sizeof(failure)); }
    while (received < 0 && errno == EINTR);
    close(channel[0]);
    if (received != 0) {
        if (received < 0) failure = errno;
        if (!failure) failure = EIO;
        kill(pid, SIGKILL);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {}
        goto done;
    }
    children[slot].pid = pid;
    children[slot].group = fields[0] ? (pid_t)fields[0] : pid;
    children[slot].monitored = 0;
    if (getenv("CCLSH_SESSION_DEBUG"))
        fprintf(stderr, "cclsh-session: spawned %s pid=%ld group=%ld\n",
                program, (long)pid, (long)children[slot].group);
    free(arguments); free(environment);
    return pid;
done:
    free(arguments); free(environment);
    return -failure;
}

static int reap_children(int socket)
{
    int status, index;
    pid_t pid;
    /* Keep exited group leaders until the shell finishes constructing the
     * pipeline. Reaping them earlier can destroy a group before its next
     * stage joins, particularly with short-lived commands on NetBSD. */
    for (index = 0; index < 256; index++) {
        if (!children[index].pid || !children[index].monitored) continue;
        while ((pid = waitpid(children[index].pid, &status,
                            WNOHANG | WUNTRACED | WCONTINUED)) > 0) {
            uint32_t event[3] = {(uint32_t)pid, 0, 0};
            if (WIFEXITED(status)) { event[1] = 1; event[2] = WEXITSTATUS(status); }
            else if (WIFSIGNALED(status)) { event[1] = 2; event[2] = WTERMSIG(status); }
            else if (WIFSTOPPED(status)) { event[1] = 3; event[2] = WSTOPSIG(status); }
            if (getenv("CCLSH_SESSION_DEBUG"))
                fprintf(stderr, "cclsh-session: child %ld state=%u code=%u\n",
                        (long)pid, event[1], event[2]);
            if (sh_send(socket, sh_child, 0, event, sizeof(event), NULL, 0) < 0) return -1;
            if (event[1] == 1 || event[1] == 2) {
                children[index].pid = 0;
                break;
            }
        }
    }
    return 0;
}

static int session(int socket)
{
    char body[sh_capacity], *cwd = getcwd(NULL, 0), **entry;
    uint32_t length = 4, type, token, count, group = (uint32_t)getpgrp();
    int fds[sh_descriptor_limit] = {0, 1, 2}, result = -1, index, orderly = 0;
    struct termios saved;
    struct sigaction action = {0};
    if (!cwd || tcgetattr(0, &saved) < 0) { free(cwd); return -1; }
    memcpy(body, &group, 4);
    if (strlen(cwd) + 1 > sizeof(body) - length) { free(cwd); return -1; }
    strcpy(body + length, cwd); length += (uint32_t)strlen(cwd) + 1;
    free(cwd);
    for (entry = environ; *entry; entry++) {
        size_t size = strlen(*entry) + 1;
        if (size > sizeof(body) - length) return -1;
        memcpy(body + length, *entry, size); length += (uint32_t)size;
    }
    if (sh_send(socket, sh_hello, 0, body, length, fds, 3) < 0 ||
        sh_receive(socket, &type, &token, body, &length, fds, &count) < 0)
        return -1;
    while (count) close(fds[--count]);
    if (type != sh_accept) return -1;
    if (pipe(wake_pipe) < 0) return -1;
    for (index = 0; index < 2; index++) {
        fcntl(wake_pipe[index], F_SETFL, O_NONBLOCK);
        fcntl(wake_pipe[index], F_SETFD, FD_CLOEXEC);
    }
    action.sa_handler = notify;
    sigemptyset(&action.sa_mask);
    sigaction(SIGCHLD, &action, NULL); sigaction(SIGHUP, &action, NULL);
    sigaction(SIGTERM, &action, NULL); sigaction(SIGINT, &action, NULL);
    sigaction(SIGQUIT, &action, NULL);
    signal(SIGTTOU, SIG_IGN); signal(SIGTTIN, SIG_IGN);
    /* Once committed, failure must never execute the command a second time. */
    result = 70;
    if (sh_send(socket, sh_accept, 0, NULL, 0, NULL, 0) < 0) goto done;
    {
        struct timeval timeout = {0, 0};
        setsockopt(socket, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
        setsockopt(socket, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));
    }
    while (!stopping) {
        struct pollfd items[] = {{socket, POLLIN, 0}, {wake_pipe[0], POLLIN, 0}};
        int ready;
        if (reap_children(socket) < 0) break;
        ready = poll(items, 2, -1);
        if (ready < 0 && errno != EINTR) break;
        if (items[1].revents & POLLIN) {
            while (read(wake_pipe[0], body, sizeof(body)) > 0) {}
        }
        if (reap_children(socket) < 0) break;
        if (items[0].revents & POLLIN) {
            int32_t answer = -EPROTO;
            int opened = -1;
            if (sh_receive(socket, &type, &token, body, &length, fds, &count) < 0) break;
            if (type == sh_spawn) answer = spawn_child(body, length, fds, count);
            else if (type == sh_foreground && length == 4) {
                int32_t target; memcpy(&target, body, 4);
                answer = tcsetpgrp(0, target) < 0 ? -errno : 0;
            } else if (type == sh_kill && length == 8) {
                int32_t fields[2]; memcpy(fields, body, 8);
                answer = kill(fields[0], fields[1]) < 0 ? -errno : 0;
            } else if (type == sh_exit && length == 4) {
                memcpy(&result, body, 4);
                orderly = 1;
                while (count) close(fds[--count]);
                goto done;
            } else if (type == sh_open && length > 8 && body[length - 1] == 0) {
                uint32_t fields[2]; memcpy(fields, body, 8);
                opened = open(body + 8, (int)fields[0] | O_CLOEXEC, fields[1]);
                answer = opened < 0 ? -errno : 0;
            } else if (type == sh_monitor && length == 4) {
                uint32_t pid; memcpy(&pid, body, 4);
                for (index = 0; index < 256; index++)
                    if (children[index].pid == (pid_t)pid)
                        children[index].monitored = 1;
                answer = 0;
            }
            while (count) close(fds[--count]);
            {
                int sent = sh_send(socket, sh_reply, token, &answer, 4,
                                   &opened, opened >= 0 ? 1 : 0);
                if (opened >= 0) close(opened);
                if (sent < 0) break;
            }
        } else if (items[0].revents & (POLLHUP | POLLERR | POLLNVAL)) break;
    }
done:
    for (index = 0; index < 256; index++) {
        if (!orderly && children[index].pid && children[index].group != getpgrp()) {
            kill(-children[index].group, SIGHUP);
            kill(-children[index].group, SIGCONT);
        }
    }
    tcsetpgrp(0, getpgrp());
    tcsetattr(0, TCSANOW, &saved);
    close(wake_pipe[0]); close(wake_pipe[1]);
    return stopping ? 128 + stopping : result;
}

static int management(const char *path, int stop, int quiet)
{
    char body[sh_capacity];
    int fds[8], socket = sh_connect(path);
    uint32_t type, token, length, count;
    if (socket < 0) return 1;
    if (sh_send(socket, stop ? sh_stop : sh_status, 0, NULL, 0, NULL, 0) < 0 ||
        sh_receive(socket, &type, &token, body, &length, fds, &count) < 0) {
        close(socket); return 1;
    }
    while (count) close(fds[--count]);
    if (!quiet) fwrite(body, 1, length, stdout);
    close(socket);
    return type == sh_reply ? 0 : 1;
}

int main(int argc, char **argv)
{
    int socket, result;
    signal(SIGPIPE, SIG_IGN);
    if (argc == 3 && (!strcmp(argv[1], "status") || !strcmp(argv[1], "stop")))
        return management(argv[2], !strcmp(argv[1], "stop"), 0);
    if (argc == 4 && !strcmp(argv[1], "start")) {
        pid_t pid;
        int index;
        if (management(argv[2], 0, 1) == 0) return 0;
        pid = fork();
        if (pid < 0) return 1;
        if (pid == 0) {
            int null = open("/dev/null", O_RDWR);
            setsid();
            dup2(null, 0); dup2(null, 1); dup2(null, 2);
            close_after_three(); close(3);
            execl(argv[3], argv[3], "run", argv[2], (char *)NULL);
            _exit(127);
        }
        for (index = 0; index < 300; index++) {
            if (management(argv[2], 0, 1) == 0) return 0;
            if (waitpid(pid, NULL, WNOHANG) == pid) {
                fprintf(stderr, "cclshd: server startup failed; run scripts/cclshd run for diagnostics\n");
                return 1;
            }
            poll(NULL, 0, 100);
        }
        fprintf(stderr, "cclshd: server startup timed out\n");
        return 1;
    }
    if (argc < 4 || strcmp(argv[1], "attach")) {
        fprintf(stderr, "usage: cclsh-session attach SOCKET FALLBACK [ARG ...]\n"
                        "       cclsh-session start SOCKET LAUNCHER\n"
                        "       cclsh-session status|stop SOCKET\n");
        return 2;
    }
    if (argc == 4 && isatty(0) && isatty(1) && isatty(2) &&
        tcgetpgrp(0) == getpgrp() && (socket = sh_connect(argv[2])) >= 0) {
        result = session(socket);
        close(socket);
        if (result >= 0) return result;
    }
    signal(SIGPIPE, SIG_DFL);
    execv(argv[3], argv + 3);
    perror("cclsh-session: fallback");
    return 127;
}
