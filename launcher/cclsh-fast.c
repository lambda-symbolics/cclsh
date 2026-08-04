#define _GNU_SOURCE

#include <arpa/inet.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <poll.h>
#include <pty.h>
#include <sched.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

extern char **environ;

/*
 * The local protocol uses big-endian fixed-width fields. A request starts
 * with magic, version, type, body and field lengths, umask and flags through
 * byte 31. Kernel, image and broker file identities occupy bytes 32, 64 and
 * 96; the caller's cwd device and inode occupy bytes 128 and 136. Replies
 * carry their code and worker counts through byte 15, followed by the three
 * file identities at bytes 16, 48 and 80. The variable request body is cwd,
 * runtime path and NUL-terminated environment entries in that order.
 */
enum {
    request_magic = 0x43434631U,
    request_version = 4,
    request_lease = 1,
    request_status = 2,
    request_stop = 3,
    request_header_size = 144,
    request_body_limit = 131072,
    reply_size = 112,
    reply_offer = 0,
    reply_unavailable = 1,
    reply_mismatch = 2,
    reply_bad_request = 3,
    reply_status = 4,
    reply_stopped = 5,
    reply_committed = 6,
    worker_unused = 0,
    worker_starting = 1,
    worker_ready = 2,
    worker_preparing = 3,
    worker_active = 4,
    max_workers = 16
};

struct worker {
    pid_t pid;
    int master_fd;
    int control_fd;
    int client_fd;
    int state;
    int64_t started_at;
};

struct file_identity {
    uint64_t device;
    uint64_t inode;
    uint64_t size;
    uint64_t modified_nanoseconds;
};

static char runtime_path[PATH_MAX];
static char runtime_image_path[PATH_MAX];
static char runtime_directory[PATH_MAX];
static char socket_path[PATH_MAX];
static char lock_path[PATH_MAX];
static struct file_identity runtime_identity;
static struct file_identity runtime_image_identity;
static struct file_identity broker_identity;
static uid_t own_uid;
static gid_t own_gid;
static volatile sig_atomic_t daemon_stop_requested;
static volatile sig_atomic_t relay_signal;
static volatile sig_atomic_t relay_resize;

static void close_inherited_descriptors(int preserved_descriptor);
static void reset_relay_handlers(void);

static uint64_t
host_to_be64(uint64_t value)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return __builtin_bswap64(value);
#else
    return value;
#endif
}

static int64_t
monotonic_milliseconds(void)
{
    struct timespec value;
    if (clock_gettime(CLOCK_MONOTONIC, &value) < 0) {
        return -1;
    }
    return (int64_t)value.tv_sec * 1000 + value.tv_nsec / 1000000;
}

static uint64_t
be64_to_host(uint64_t value)
{
    return host_to_be64(value);
}

static void
put_u16(unsigned char *buffer, size_t offset, uint16_t value)
{
    value = htons(value);
    memcpy(buffer + offset, &value, sizeof(value));
}

static void
put_u32(unsigned char *buffer, size_t offset, uint32_t value)
{
    value = htonl(value);
    memcpy(buffer + offset, &value, sizeof(value));
}

static void
put_u64(unsigned char *buffer, size_t offset, uint64_t value)
{
    value = host_to_be64(value);
    memcpy(buffer + offset, &value, sizeof(value));
}

static uint16_t
get_u16(const unsigned char *buffer, size_t offset)
{
    uint16_t value;
    memcpy(&value, buffer + offset, sizeof(value));
    return ntohs(value);
}

static uint32_t
get_u32(const unsigned char *buffer, size_t offset)
{
    uint32_t value;
    memcpy(&value, buffer + offset, sizeof(value));
    return ntohl(value);
}

static uint64_t
get_u64(const unsigned char *buffer, size_t offset)
{
    uint64_t value;
    memcpy(&value, buffer + offset, sizeof(value));
    return be64_to_host(value);
}

static int
set_close_on_exec(int descriptor)
{
    int flags = fcntl(descriptor, F_GETFD);
    if (flags < 0) {
        return -1;
    }
    return fcntl(descriptor, F_SETFD, flags | FD_CLOEXEC);
}

static int
set_nonblocking(int descriptor)
{
    int flags = fcntl(descriptor, F_GETFL);
    if (flags < 0) {
        return -1;
    }
    return fcntl(descriptor, F_SETFL, flags | O_NONBLOCK);
}

static int
write_all(int descriptor, const void *data, size_t length)
{
    const unsigned char *cursor = data;
    while (length > 0) {
        ssize_t written = write(descriptor, cursor, length);
        if (written < 0) {
            if (errno == EINTR) {
                continue;
            }
            return -1;
        }
        if (written == 0) {
            errno = EPIPE;
            return -1;
        }
        cursor += (size_t)written;
        length -= (size_t)written;
    }
    return 0;
}

static int
read_byte_with_timeout(int descriptor, unsigned char *value, int timeout_ms)
{
    struct pollfd item = {descriptor, POLLIN, 0};
    int64_t deadline = monotonic_milliseconds() + timeout_ms;
    for (;;) {
        int64_t now = monotonic_milliseconds();
        int remaining;
        int result;
        if (now < 0 || deadline < now) {
            return -1;
        }
        remaining = (int)(deadline - now);
        result = poll(&item, 1, remaining);
        if (result < 0 && errno == EINTR) {
            continue;
        }
        if (result <= 0) {
            return -1;
        }
        if (item.revents & POLLIN) {
            ssize_t count = read(descriptor, value, 1);
            return count == 1 ? 0 : -1;
        }
        if (item.revents & (POLLERR | POLLHUP | POLLNVAL)) {
            return -1;
        }
    }
}

static uint64_t
stat_modified_nanoseconds(const struct stat *status)
{
    return (uint64_t)status->st_mtim.tv_sec * UINT64_C(1000000000) +
           (uint64_t)status->st_mtim.tv_nsec;
}

static int
read_file_identity(const char *path, struct file_identity *identity)
{
    struct stat status;
    if (stat(path, &status) < 0 || !S_ISREG(status.st_mode)) {
        return -1;
    }
    identity->device = (uint64_t)status.st_dev;
    identity->inode = (uint64_t)status.st_ino;
    identity->size = (uint64_t)status.st_size;
    identity->modified_nanoseconds = stat_modified_nanoseconds(&status);
    return 0;
}

static int
worker_directory_matches(const struct worker *worker,
                         const unsigned char *packet)
{
    char path[PATH_MAX];
    struct stat status;
    if (worker->pid <= 0 ||
        snprintf(path, sizeof(path), "/proc/%ld/cwd", (long)worker->pid) >=
            (int)sizeof(path) ||
        stat(path, &status) < 0 || !S_ISDIR(status.st_mode)) {
        return 0;
    }
    return get_u64(packet, 128) == (uint64_t)status.st_dev &&
           get_u64(packet, 136) == (uint64_t)status.st_ino;
}

static int
file_identity_equal(const struct file_identity *left,
                    const struct file_identity *right)
{
    return left->device == right->device && left->inode == right->inode &&
           left->size == right->size &&
           left->modified_nanoseconds == right->modified_nanoseconds;
}

static int
packet_identity_equal(const unsigned char *packet, size_t offset,
                      const struct file_identity *identity)
{
    return get_u64(packet, offset) == identity->device &&
           get_u64(packet, offset + 8) == identity->inode &&
           get_u64(packet, offset + 16) == identity->size &&
           get_u64(packet, offset + 24) == identity->modified_nanoseconds;
}

static void
put_file_identity(unsigned char *packet, size_t offset,
                  const struct file_identity *identity)
{
    put_u64(packet, offset, identity->device);
    put_u64(packet, offset + 8, identity->inode);
    put_u64(packet, offset + 16, identity->size);
    put_u64(packet, offset + 24, identity->modified_nanoseconds);
}

static int
refresh_runtime_identity(void)
{
    struct stat status;
    if (read_file_identity(runtime_path, &runtime_identity) < 0 ||
        stat(runtime_path, &status) < 0 || !(status.st_mode & S_IXUSR) ||
        read_file_identity(runtime_image_path, &runtime_image_identity) < 0) {
        return -1;
    }
    return 0;
}

static int
initialize_runtime_path(void)
{
    char executable[PATH_MAX];
    char copy[PATH_MAX];
    ssize_t length = readlink("/proc/self/exe", executable,
                              sizeof(executable) - 1);
    if (length < 0 || (size_t)length >= sizeof(executable) - 1) {
        return -1;
    }
    executable[length] = '\0';
    if (read_file_identity(executable, &broker_identity) < 0) {
        return -1;
    }
    if (snprintf(copy, sizeof(copy), "%s", executable) >= (int)sizeof(copy)) {
        return -1;
    }
    if (snprintf(runtime_path, sizeof(runtime_path), "%s/cclsh",
                 dirname(copy)) >= (int)sizeof(runtime_path)) {
        return -1;
    }
    if (snprintf(runtime_image_path, sizeof(runtime_image_path), "%s.image",
                 runtime_path) >= (int)sizeof(runtime_image_path) ||
        runtime_path[0] != '/' || refresh_runtime_identity() < 0) {
        return -1;
    }

    return 0;
}

static int
initialize_daemon_paths(void)
{
    const char *xdg_runtime;
    struct stat status;
    own_uid = geteuid();
    own_gid = getegid();
    xdg_runtime = getenv("XDG_RUNTIME_DIR");
    if (xdg_runtime == NULL || xdg_runtime[0] != '/' ||
        lstat(xdg_runtime, &status) < 0 || !S_ISDIR(status.st_mode) ||
        S_ISLNK(status.st_mode) || status.st_uid != own_uid ||
        (status.st_mode & 0077) != 0) {
        return -1;
    }
    if (snprintf(runtime_directory, sizeof(runtime_directory),
                 "%s/cclsh-fast", xdg_runtime) >=
        (int)sizeof(runtime_directory)) {
        return -1;
    }
    if (mkdir(runtime_directory, 0700) < 0 && errno != EEXIST) {
        return -1;
    }
    if (lstat(runtime_directory, &status) < 0 ||
        !S_ISDIR(status.st_mode) || S_ISLNK(status.st_mode) ||
        status.st_uid != own_uid || (status.st_mode & 0777) != 0700) {
        return -1;
    }
    if (snprintf(socket_path, sizeof(socket_path), "%s/supervisor.sock",
                 runtime_directory) >= (int)sizeof(socket_path) ||
        snprintf(lock_path, sizeof(lock_path), "%s/supervisor.lock",
                 runtime_directory) >= (int)sizeof(lock_path)) {
        return -1;
    }
    return 0;
}

static int
set_socket_timeouts(int descriptor, int milliseconds)
{
    struct timeval timeout;
    timeout.tv_sec = milliseconds / 1000;
    timeout.tv_usec = (__suseconds_t)(milliseconds % 1000) * 1000;
    if (setsockopt(descriptor, SOL_SOCKET, SO_RCVTIMEO,
                   &timeout, sizeof(timeout)) < 0 ||
        setsockopt(descriptor, SOL_SOCKET, SO_SNDTIMEO,
                   &timeout, sizeof(timeout)) < 0) {
        return -1;
    }
    return 0;
}

static void
direct_exec(int argc, char **argv)
{
    char **arguments;
    int index;
    arguments = (char **)calloc((size_t)argc + 1, sizeof(*arguments));
    if (arguments == NULL) {
        _exit(127);
    }
    arguments[0] = runtime_path;
    for (index = 1; index < argc; index++) {
        arguments[index] = argv[index];
    }
    execv(runtime_path, arguments);
    dprintf(STDERR_FILENO, "cclsh-fast: cannot start %s: %s\n",
            runtime_path, strerror(errno));
    _exit(127);
}

static int
socket_metadata_safe(void)
{
    struct stat status;
    if (lstat(socket_path, &status) < 0) {
        return 0;
    }
    return S_ISSOCK(status.st_mode) && !S_ISLNK(status.st_mode) &&
           status.st_uid == own_uid && (status.st_mode & 0077) == 0;
}

static int
connect_supervisor(int timeout_ms)
{
    int descriptor;
    int result;
    int error = 0;
    socklen_t error_length = sizeof(error);
    struct sockaddr_un address;
    struct ucred credentials;
    socklen_t credentials_length = sizeof(credentials);
    if (!socket_metadata_safe()) {
        return -1;
    }
    descriptor = socket(AF_UNIX,
                        SOCK_SEQPACKET | SOCK_CLOEXEC | SOCK_NONBLOCK, 0);
    if (descriptor < 0) {
        return -1;
    }
    memset(&address, 0, sizeof(address));
    address.sun_family = AF_UNIX;
    if (snprintf(address.sun_path, sizeof(address.sun_path), "%s",
                 socket_path) >= (int)sizeof(address.sun_path)) {
        close(descriptor);
        return -1;
    }
    result = connect(descriptor, (struct sockaddr *)&address, sizeof(address));
    if (result < 0 && errno == EINPROGRESS) {
        struct pollfd item = {descriptor, POLLOUT, 0};
        int64_t deadline = monotonic_milliseconds() + timeout_ms;
        do {
            int64_t now = monotonic_milliseconds();
            int remaining;
            if (now < 0 || deadline < now) {
                result = -1;
                break;
            }
            remaining = (int)(deadline - now);
            result = poll(&item, 1, remaining);
        } while (result < 0 && errno == EINTR);
        if (result > 0 &&
            getsockopt(descriptor, SOL_SOCKET, SO_ERROR,
                       &error, &error_length) == 0 && error == 0) {
            result = 0;
        } else {
            result = -1;
        }
    }
    if (result < 0) {
        close(descriptor);
        return -1;
    }
    {
        int flags = fcntl(descriptor, F_GETFL);
        if (flags < 0 ||
            fcntl(descriptor, F_SETFL, flags & ~O_NONBLOCK) < 0) {
            close(descriptor);
            return -1;
        }
    }
    if (getsockopt(descriptor, SOL_SOCKET, SO_PEERCRED, &credentials,
                   &credentials_length) < 0 ||
        credentials.uid != own_uid || credentials.gid != own_gid ||
        set_socket_timeouts(descriptor, timeout_ms) < 0) {
        close(descriptor);
        return -1;
    }
    return descriptor;
}

static void
make_header(unsigned char *header, uint16_t type, uint32_t body_length,
            uint32_t cwd_length, uint32_t shell_length,
            uint32_t environment_length, uint32_t mask)
{
    memset(header, 0, request_header_size);
    put_u32(header, 0, request_magic);
    put_u16(header, 4, request_version);
    put_u16(header, 6, type);
    put_u32(header, 8, body_length);
    put_u32(header, 12, cwd_length);
    put_u32(header, 16, shell_length);
    put_u32(header, 20, environment_length);
    put_u32(header, 24, mask);
    put_u32(header, 28, 0);
    put_file_identity(header, 32, &runtime_identity);
    put_file_identity(header, 64, &runtime_image_identity);
    put_file_identity(header, 96, &broker_identity);
}

static void
make_reply(unsigned char *reply, uint16_t code, uint32_t ready,
           uint32_t active)
{
    memset(reply, 0, reply_size);
    put_u32(reply, 0, request_magic);
    put_u16(reply, 4, request_version);
    put_u16(reply, 6, code);
    put_u32(reply, 8, ready);
    put_u32(reply, 12, active);
    put_file_identity(reply, 16, &runtime_identity);
    put_file_identity(reply, 48, &runtime_image_identity);
    put_file_identity(reply, 80, &broker_identity);
}

static int
send_reply(int descriptor, uint16_t code, uint32_t ready, uint32_t active)
{
    unsigned char reply[reply_size];
    make_reply(reply, code, ready, active);
    return send(descriptor, reply, sizeof(reply), MSG_NOSIGNAL) ==
                   (ssize_t)sizeof(reply)
               ? 0
               : -1;
}

static int
send_offer(int descriptor, int master_fd, uint32_t ready, uint32_t active)
{
    unsigned char reply[reply_size];
    unsigned char control[CMSG_SPACE(sizeof(int))];
    struct iovec vector;
    struct msghdr message;
    struct cmsghdr *header;
    make_reply(reply, reply_offer, ready, active);
    memset(&message, 0, sizeof(message));
    memset(control, 0, sizeof(control));
    vector.iov_base = reply;
    vector.iov_len = sizeof(reply);
    message.msg_iov = &vector;
    message.msg_iovlen = 1;
    message.msg_control = control;
    message.msg_controllen = sizeof(control);
    header = CMSG_FIRSTHDR(&message);
    header->cmsg_level = SOL_SOCKET;
    header->cmsg_type = SCM_RIGHTS;
    header->cmsg_len = CMSG_LEN(sizeof(int));
    memcpy(CMSG_DATA(header), &master_fd, sizeof(master_fd));
    return sendmsg(descriptor, &message, MSG_NOSIGNAL) ==
                   (ssize_t)sizeof(reply)
               ? 0
               : -1;
}

static int
receive_reply(int descriptor, unsigned char *reply, int *received_fd)
{
    unsigned char control[CMSG_SPACE(sizeof(int) * 2)];
    struct iovec vector;
    struct msghdr message;
    struct cmsghdr *header;
    ssize_t count;
    int descriptors = 0;
    *received_fd = -1;
    memset(&message, 0, sizeof(message));
    memset(control, 0, sizeof(control));
    vector.iov_base = reply;
    vector.iov_len = reply_size;
    message.msg_iov = &vector;
    message.msg_iovlen = 1;
    message.msg_control = control;
    message.msg_controllen = sizeof(control);
    do {
        message.msg_controllen = sizeof(control);
        message.msg_flags = 0;
        count = recvmsg(descriptor, &message, MSG_CMSG_CLOEXEC);
    } while (count < 0 && errno == EINTR);
    if (count != reply_size || (message.msg_flags & (MSG_TRUNC | MSG_CTRUNC))) {
        return -1;
    }
    for (header = CMSG_FIRSTHDR(&message); header != NULL;
         header = CMSG_NXTHDR(&message, header)) {
        if (header->cmsg_level == SOL_SOCKET &&
            header->cmsg_type == SCM_RIGHTS) {
            if (header->cmsg_len < CMSG_LEN(sizeof(int))) {
                if (*received_fd >= 0) {
                    close(*received_fd);
                    *received_fd = -1;
                }
                return -1;
            }
            size_t payload = header->cmsg_len - CMSG_LEN(0);
            size_t amount = payload / sizeof(int);
            size_t index;
            int *values = (int *)CMSG_DATA(header);
            for (index = 0; index < amount; index++) {
                if (descriptors == 0) {
                    *received_fd = values[index];
                } else {
                    close(values[index]);
                }
                descriptors++;
            }
        }
    }
    return 0;
}

static int
validate_reply_envelope(const unsigned char *reply)
{
    return get_u32(reply, 0) == request_magic &&
           get_u16(reply, 4) == request_version;
}

static int
validate_reply(const unsigned char *reply)
{
    return validate_reply_envelope(reply) &&
           packet_identity_equal(reply, 16, &runtime_identity) &&
           packet_identity_equal(reply, 48, &runtime_image_identity) &&
           packet_identity_equal(reply, 80, &broker_identity);
}

static int
worker_count(struct worker *workers, int state)
{
    int count = 0;
    int index;
    for (index = 0; index < max_workers; index++) {
        if (workers[index].state == state) {
            count++;
        }
    }
    return count;
}

static int
worker_find_slot(struct worker *workers)
{
    int index;
    for (index = 0; index < max_workers; index++) {
        if (workers[index].state == worker_unused) {
            return index;
        }
    }
    return -1;
}

static void
worker_close_fds(struct worker *worker)
{
    if (worker->master_fd >= 0) {
        close(worker->master_fd);
    }
    if (worker->control_fd >= 0) {
        close(worker->control_fd);
    }
    if (worker->client_fd >= 0) {
        close(worker->client_fd);
    }
    worker->master_fd = -1;
    worker->control_fd = -1;
    worker->client_fd = -1;
}

static void
worker_signal_foreground(struct worker *worker, int signal_number)
{
    pid_t foreground = -1;
    if (worker->master_fd >= 0) {
        foreground = tcgetpgrp(worker->master_fd);
    }
    if (foreground > 0) {
        kill(-foreground, signal_number);
    }
    if (worker->pid > 0 && foreground != worker->pid) {
        kill(-worker->pid, signal_number);
    }
}

static void
worker_signal_session(struct worker *worker, int signal_number)
{
    DIR *directory;
    struct dirent *entry;
    if (worker->pid <= 0 || (directory = opendir("/proc")) == NULL) {
        return;
    }
    while ((entry = readdir(directory)) != NULL) {
        char *end = NULL;
        long process;
        errno = 0;
        process = strtol(entry->d_name, &end, 10);
        if (errno == 0 && end != entry->d_name && *end == '\0' &&
            process > 0 && getsid((pid_t)process) == worker->pid) {
            kill((pid_t)process, signal_number);
        }
    }
    closedir(directory);
}

static void
worker_discard(struct worker *worker)
{
    if (worker->pid > 0) {
        worker_signal_foreground(worker, SIGHUP);
        worker_signal_session(worker, SIGHUP);
        worker_signal_foreground(worker, SIGCONT);
        worker_signal_session(worker, SIGCONT);
        worker_signal_session(worker, SIGKILL);
        worker_signal_foreground(worker, SIGKILL);
        kill(worker->pid, SIGKILL);
    }
    worker_close_fds(worker);
    worker->state = worker_unused;
    worker->pid = 0;
}

static int
worker_spawn(struct worker *workers)
{
    int slot = worker_find_slot(workers);
    int control[2];
    int master = -1;
    pid_t pid;
    char descriptor_text[32];
    if (slot < 0 || socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0,
                               control) < 0) {
        return -1;
    }
    pid = forkpty(&master, NULL, NULL, NULL);
    if (pid < 0) {
        close(control[0]);
        close(control[1]);
        return -1;
    }
    if (pid == 0) {
        struct sigaction default_action;
        sigset_t empty_mask;
        int signal_number;
        if (control[0] != control[1]) {
            close(control[0]);
        }
        if (control[1] != 3) {
            if (dup2(control[1], 3) < 0) {
                _exit(126);
            }
            close(control[1]);
        }
        if (fcntl(3, F_SETFD, 0) < 0) {
            _exit(126);
        }
        snprintf(descriptor_text, sizeof(descriptor_text), "%d", 3);
        if (setenv("CCLSH_FAST_WORKER_FD", descriptor_text, 1) < 0) {
            _exit(126);
        }
        memset(&default_action, 0, sizeof(default_action));
        default_action.sa_handler = SIG_DFL;
        sigemptyset(&default_action.sa_mask);
        for (signal_number = 1; signal_number < NSIG; signal_number++) {
            if (signal_number != SIGKILL && signal_number != SIGSTOP) {
                sigaction(signal_number, &default_action, NULL);
            }
        }
        sigemptyset(&empty_mask);
        sigprocmask(SIG_SETMASK, &empty_mask, NULL);
        execl(runtime_path, runtime_path,
              "--cclsh-internal-prewarm-worker", (char *)NULL);
        _exit(127);
    }
    close(control[1]);
    if (set_close_on_exec(master) < 0 ||
        set_socket_timeouts(control[0], 750) < 0) {
        close(master);
        close(control[0]);
        kill(-pid, SIGHUP);
        kill(pid, SIGKILL);
        return -1;
    }
    workers[slot].pid = pid;
    workers[slot].master_fd = master;
    workers[slot].control_fd = control[0];
    workers[slot].client_fd = -1;
    workers[slot].state = worker_starting;
    workers[slot].started_at = monotonic_milliseconds();
    return 0;
}

static void
worker_reap(struct worker *workers)
{
    int status;
    pid_t pid;
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
        int index;
        for (index = 0; index < max_workers; index++) {
            struct worker *worker = &workers[index];
            if (worker->state == worker_unused || worker->pid != pid) {
                continue;
            }
            if (worker->state == worker_active && worker->client_fd >= 0) {
                uint32_t code;
                unsigned char encoded[4];
                if (WIFEXITED(status)) {
                    code = (uint32_t)WEXITSTATUS(status);
                } else if (WIFSIGNALED(status)) {
                    code = (uint32_t)(128 + WTERMSIG(status));
                } else {
                    code = 70;
                }
                put_u32(encoded, 0, code);
                send(worker->client_fd, encoded, sizeof(encoded), MSG_NOSIGNAL);
                worker_signal_foreground(worker, SIGHUP);
                worker_signal_foreground(worker, SIGCONT);
            }
            worker_signal_session(worker, SIGHUP);
            worker_signal_session(worker, SIGCONT);
            worker_signal_session(worker, SIGKILL);
            worker_close_fds(worker);
            memset(worker, 0, sizeof(*worker));
            worker->master_fd = -1;
            worker->control_fd = -1;
            worker->client_fd = -1;
            break;
        }
    }
}

static void
worker_poll_starting(struct worker *workers)
{
    int index;
    for (index = 0; index < max_workers; index++) {
        struct worker *worker = &workers[index];
        struct pollfd items[2];
        unsigned char value;
        int result;
        if (worker->state != worker_starting) {
            continue;
        }
        if (worker->started_at < 0 ||
            monotonic_milliseconds() - worker->started_at > 3000) {
            worker_discard(worker);
            continue;
        }
        items[0].fd = worker->control_fd;
        items[0].events = POLLIN;
        items[0].revents = 0;
        items[1].fd = worker->master_fd;
        items[1].events = POLLIN;
        items[1].revents = 0;
        result = poll(items, 2, 0);
        if (result > 0) {
            if (items[1].revents &
                (POLLIN | POLLHUP | POLLERR | POLLNVAL)) {
                worker_discard(worker);
            } else if ((items[0].revents & POLLIN) &&
                read(worker->control_fd, &value, 1) == 1 && value == 'R') {
                worker->state = worker_ready;
            } else if (items[0].revents &
                       (POLLIN | POLLHUP | POLLERR | POLLNVAL)) {
                worker_discard(worker);
            }
        }
    }
}

static int
worker_master_is_clean(const struct worker *worker)
{
    struct pollfd item = {worker->master_fd, POLLIN, 0};
    int result;
    do {
        result = poll(&item, 1, 0);
    } while (result < 0 && errno == EINTR);
    return result == 0;
}

static void
ensure_ready_worker(struct worker *workers)
{
    if (worker_count(workers, worker_ready) == 0 &&
        worker_count(workers, worker_starting) == 0) {
        worker_spawn(workers);
    }
}

static int
request_valid(const unsigned char *packet, size_t length)
{
    uint32_t body_length;
    uint32_t cwd_length;
    uint32_t shell_length;
    uint32_t environment_length;
    uint64_t field_total;
    if (length < request_header_size ||
        get_u32(packet, 0) != request_magic ||
        get_u16(packet, 4) != request_version) {
        return 0;
    }
    body_length = get_u32(packet, 8);
    cwd_length = get_u32(packet, 12);
    shell_length = get_u32(packet, 16);
    environment_length = get_u32(packet, 20);
    field_total = (uint64_t)cwd_length + (uint64_t)shell_length +
                  (uint64_t)environment_length;
    return body_length <= request_body_limit &&
           cwd_length <= request_body_limit &&
           shell_length <= request_body_limit &&
           environment_length <= request_body_limit &&
           field_total == body_length &&
           length == request_header_size + body_length;
}

static int
peer_is_owner(int descriptor)
{
    struct ucred credentials;
    socklen_t length = sizeof(credentials);
    return getsockopt(descriptor, SOL_SOCKET, SO_PEERCRED, &credentials,
                      &length) == 0 && credentials.uid == own_uid &&
           credentials.gid == own_gid;
}

static int
read_small_file(const char *path, unsigned char *buffer, size_t capacity,
                size_t *length)
{
    int descriptor = open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    size_t total = 0;
    if (descriptor < 0) {
        return -1;
    }
    while (total < capacity) {
        ssize_t count = read(descriptor, buffer + total, capacity - total);
        if (count < 0 && errno == EINTR) {
            continue;
        }
        if (count < 0) {
            close(descriptor);
            return -1;
        }
        if (count == 0) {
            close(descriptor);
            *length = total;
            return 0;
        }
        total += (size_t)count;
    }
    close(descriptor);
    errno = EOVERFLOW;
    return -1;
}

static int
proc_files_match(pid_t peer, const char *name)
{
    char self_path[PATH_MAX];
    char peer_path[PATH_MAX];
    unsigned char self_buffer[65536];
    unsigned char peer_buffer[65536];
    size_t self_length;
    size_t peer_length;
    if (snprintf(self_path, sizeof(self_path), "/proc/self/%s", name) >=
            (int)sizeof(self_path) ||
        snprintf(peer_path, sizeof(peer_path), "/proc/%ld/%s",
                 (long)peer, name) >= (int)sizeof(peer_path)) {
        return 0;
    }
    if (read_small_file(self_path, self_buffer, sizeof(self_buffer),
                        &self_length) < 0) {
        return read_small_file(peer_path, peer_buffer, sizeof(peer_buffer),
                               &peer_length) < 0;
    }
    return read_small_file(peer_path, peer_buffer, sizeof(peer_buffer),
                           &peer_length) == 0 &&
           self_length == peer_length &&
           memcmp(self_buffer, peer_buffer, self_length) == 0;
}

static int
proc_objects_match(pid_t peer, const char *name)
{
    char self_path[PATH_MAX];
    char peer_path[PATH_MAX];
    struct stat self_status;
    struct stat peer_status;
    if (snprintf(self_path, sizeof(self_path), "/proc/self/%s", name) >=
            (int)sizeof(self_path) ||
        snprintf(peer_path, sizeof(peer_path), "/proc/%ld/%s",
                 (long)peer, name) >= (int)sizeof(peer_path) ||
        stat(self_path, &self_status) < 0 || stat(peer_path, &peer_status) < 0) {
        return 0;
    }
    return self_status.st_dev == peer_status.st_dev &&
           self_status.st_ino == peer_status.st_ino;
}

static int
status_security_lines_match(pid_t peer)
{
    static const char *prefixes[] = {
        "Uid:", "Gid:", "Groups:", "CapInh:", "CapPrm:", "CapEff:",
        "CapBnd:", "CapAmb:", "NoNewPrivs:", "Seccomp:",
        "Seccomp_filters:"
    };
    char self_path[] = "/proc/self/status";
    char peer_path[PATH_MAX];
    FILE *self_file;
    FILE *peer_file;
    char *self_line = NULL;
    char *peer_line = NULL;
    size_t self_capacity = 0;
    size_t peer_capacity = 0;
    size_t index;
    int matches = 1;
    if (snprintf(peer_path, sizeof(peer_path), "/proc/%ld/status",
                 (long)peer) >= (int)sizeof(peer_path)) {
        return 0;
    }
    self_file = fopen(self_path, "re");
    peer_file = fopen(peer_path, "re");
    if (self_file == NULL || peer_file == NULL) {
        if (self_file != NULL) {
            fclose(self_file);
        }
        if (peer_file != NULL) {
            fclose(peer_file);
        }
        return 0;
    }
    for (index = 0; index < sizeof(prefixes) / sizeof(prefixes[0]); index++) {
        ssize_t self_length = -1;
        ssize_t peer_length = -1;
        if (fseek(self_file, 0, SEEK_SET) < 0 ||
            fseek(peer_file, 0, SEEK_SET) < 0) {
            matches = 0;
            break;
        }
        while (getline(&self_line, &self_capacity, self_file) >= 0) {
            if (strncmp(self_line, prefixes[index], strlen(prefixes[index])) ==
                0) {
                self_length = (ssize_t)strlen(self_line);
                break;
            }
        }
        while (getline(&peer_line, &peer_capacity, peer_file) >= 0) {
            if (strncmp(peer_line, prefixes[index], strlen(prefixes[index])) ==
                0) {
                peer_length = (ssize_t)strlen(peer_line);
                break;
            }
        }
        if (self_length < 0 && peer_length < 0) {
            continue;
        }
        if (self_length != peer_length || self_length < 0 ||
            memcmp(self_line, peer_line, (size_t)self_length) != 0) {
            matches = 0;
            break;
        }
    }
    free(self_line);
    free(peer_line);
    fclose(self_file);
    fclose(peer_file);
    return matches;
}

static int
resource_context_matches(pid_t peer)
{
    int resource;
    int self_priority;
    int peer_priority;
    int self_policy;
    int peer_policy;
    cpu_set_t self_affinity;
    cpu_set_t peer_affinity;
    struct sched_param self_parameters;
    struct sched_param peer_parameters;
    for (resource = 0; resource < RLIM_NLIMITS; resource++) {
        struct rlimit self_limit;
        struct rlimit peer_limit;
        if (prlimit(0, resource, NULL, &self_limit) < 0 ||
            prlimit(peer, resource, NULL, &peer_limit) < 0 ||
            self_limit.rlim_cur != peer_limit.rlim_cur ||
            self_limit.rlim_max != peer_limit.rlim_max) {
            return 0;
        }
    }
    errno = 0;
    self_priority = getpriority(PRIO_PROCESS, 0);
    if (errno != 0) {
        return 0;
    }
    errno = 0;
    peer_priority = getpriority(PRIO_PROCESS, peer);
    if (errno != 0 || self_priority != peer_priority) {
        return 0;
    }
    self_policy = sched_getscheduler(0);
    peer_policy = sched_getscheduler(peer);
    if (self_policy < 0 || peer_policy != self_policy ||
        sched_getparam(0, &self_parameters) < 0 ||
        sched_getparam(peer, &peer_parameters) < 0 ||
        self_parameters.sched_priority != peer_parameters.sched_priority) {
        return 0;
    }
    CPU_ZERO(&self_affinity);
    CPU_ZERO(&peer_affinity);
    if (sched_getaffinity(0, sizeof(self_affinity), &self_affinity) < 0 ||
        sched_getaffinity(peer, sizeof(peer_affinity), &peer_affinity) < 0 ||
        memcmp(&self_affinity, &peer_affinity, sizeof(self_affinity)) != 0) {
        return 0;
    }
    return 1;
}

static int
peer_context_matches(int descriptor)
{
    static const char *namespaces[] = {
        "ns/cgroup", "ns/ipc", "ns/mnt", "ns/net", "ns/pid",
        "ns/pid_for_children", "ns/time", "ns/time_for_children",
        "ns/user", "ns/uts"
    };
    struct ucred credentials;
    socklen_t length = sizeof(credentials);
    size_t index;
    if (getsockopt(descriptor, SOL_SOCKET, SO_PEERCRED, &credentials,
                   &length) < 0 || !status_security_lines_match(credentials.pid) ||
        !resource_context_matches(credentials.pid) ||
        !proc_objects_match(credentials.pid, "root") ||
        !proc_files_match(credentials.pid, "cgroup") ||
        !proc_files_match(credentials.pid, "attr/current") ||
        !proc_files_match(credentials.pid, "personality") ||
        !proc_files_match(credentials.pid, "oom_score_adj") ||
        !proc_files_match(credentials.pid, "timerslack_ns")) {
        return 0;
    }
    for (index = 0;
         index < sizeof(namespaces) / sizeof(namespaces[0]); index++) {
        if (!proc_objects_match(credentials.pid, namespaces[index])) {
            return 0;
        }
    }
    return 1;
}

static ssize_t
receive_request(int descriptor, unsigned char *packet, size_t capacity)
{
    struct iovec vector;
    struct msghdr message;
    ssize_t length;
    memset(&message, 0, sizeof(message));
    vector.iov_base = packet;
    vector.iov_len = capacity;
    message.msg_iov = &vector;
    message.msg_iovlen = 1;
    length = recvmsg(descriptor, &message, MSG_TRUNC);
    if (length < 0 || (size_t)length > capacity ||
        (message.msg_flags & (MSG_TRUNC | MSG_CTRUNC))) {
        return -1;
    }
    return length;
}

static int
find_ready_worker(struct worker *workers)
{
    int index;
    for (index = 0; index < max_workers; index++) {
        if (workers[index].state == worker_ready) {
            return index;
        }
    }
    return -1;
}

static int
exec_sensitive_environment_name(const char *name, size_t length)
{
    static const char *exact[] = {
        "GCONV_PATH", "GLIBC_TUNABLES", "HOME", "HOSTALIASES", "LANG",
        "LOCALDOMAIN", "LOCPATH", "NLSPATH", "RES_OPTIONS", "TEMP",
        "TMP", "TMPDIR", "TZ"
    };
    static const char *prefixes[] = {
        "ASAN_", "CCL_", "DYLD_", "GLIBC_", "LC_", "LD_", "LSAN_",
        "MALLOC_", "TSAN_", "UBSAN_"
    };
    size_t index;
    for (index = 0; index < sizeof(exact) / sizeof(exact[0]); index++) {
        if (strlen(exact[index]) == length &&
            memcmp(name, exact[index], length) == 0) {
            return 1;
        }
    }
    for (index = 0; index < sizeof(prefixes) / sizeof(prefixes[0]); index++) {
        size_t prefix_length = strlen(prefixes[index]);
        if (length >= prefix_length &&
            memcmp(name, prefixes[index], prefix_length) == 0) {
            return 1;
        }
    }
    return 0;
}

static const char *
environment_entry(char **environment, const char *name, size_t name_length)
{
    char **entry;
    for (entry = environment; *entry != NULL; entry++) {
        const char *equals = strchr(*entry, '=');
        if (equals != NULL && (size_t)(equals - *entry) == name_length &&
            memcmp(*entry, name, name_length) == 0) {
            return equals + 1;
        }
    }
    return NULL;
}

static int
lease_environment_compatible(const unsigned char *packet)
{
    uint32_t cwd_length = get_u32(packet, 12);
    uint32_t shell_length = get_u32(packet, 16);
    uint32_t environment_length = get_u32(packet, 20);
    const unsigned char *start = packet + request_header_size + cwd_length +
                                 shell_length;
    const unsigned char *end = start + environment_length;
    const unsigned char *cursor = start;
    char **daemon_entry;
    while (cursor < end) {
        const unsigned char *terminator = memchr(cursor, '\0',
                                                 (size_t)(end - cursor));
        const unsigned char *equals;
        const char *daemon_value;
        size_t name_length;
        if (terminator == NULL || terminator == cursor) {
            return 0;
        }
        equals = memchr(cursor, '=', (size_t)(terminator - cursor));
        if (equals == NULL || equals == cursor) {
            return 0;
        }
        name_length = (size_t)(equals - cursor);
        if (exec_sensitive_environment_name((const char *)cursor,
                                            name_length)) {
            daemon_value = environment_entry(environ, (const char *)cursor,
                                             name_length);
            if (daemon_value == NULL ||
                strlen(daemon_value) != (size_t)(terminator - equals - 1) ||
                memcmp(daemon_value, equals + 1,
                       (size_t)(terminator - equals - 1)) != 0) {
                return 0;
            }
        }
        cursor = terminator + 1;
    }
    for (daemon_entry = environ; *daemon_entry != NULL; daemon_entry++) {
        const char *equals = strchr(*daemon_entry, '=');
        size_t name_length;
        const unsigned char *request_cursor;
        int found = 0;
        if (equals == NULL || equals == *daemon_entry) {
            continue;
        }
        name_length = (size_t)(equals - *daemon_entry);
        if (!exec_sensitive_environment_name(*daemon_entry, name_length)) {
            continue;
        }
        request_cursor = start;
        while (request_cursor < end) {
            const unsigned char *terminator =
                memchr(request_cursor, '\0', (size_t)(end - request_cursor));
            if (terminator == NULL) {
                return 0;
            }
            if ((size_t)(terminator - request_cursor) > name_length &&
                memcmp(request_cursor, *daemon_entry, name_length) == 0 &&
                request_cursor[name_length] == '=') {
                found = 1;
                break;
            }
            request_cursor = terminator + 1;
        }
        if (!found) {
            return 0;
        }
    }
    return cursor == end;
}

static void
handle_lease(int client, struct worker *workers, unsigned char *packet,
             size_t length)
{
    int slot;
    struct worker *worker;
    unsigned char prepared;
    unsigned char commit;
    struct file_identity current_runtime;
    struct file_identity current_image;
    uint32_t ready = (uint32_t)worker_count(workers, worker_ready);
    uint32_t active = (uint32_t)worker_count(workers, worker_active);
    if (!packet_identity_equal(packet, 32, &runtime_identity) ||
        !packet_identity_equal(packet, 64, &runtime_image_identity) ||
        !packet_identity_equal(packet, 96, &broker_identity)) {
        send_reply(client, reply_mismatch, ready, active);
        close(client);
        return;
    }
    slot = find_ready_worker(workers);
    if (slot < 0) {
        send_reply(client, reply_unavailable, ready, active);
        close(client);
        return;
    }
    worker = &workers[slot];
    if (!worker_master_is_clean(worker)) {
        worker_discard(worker);
        close(client);
        ensure_ready_worker(workers);
        return;
    }
    worker->state = worker_preparing;
    if (write_all(worker->control_fd, packet, length) < 0 ||
        read_byte_with_timeout(worker->control_fd, &prepared, 1500) < 0 ||
        prepared != 'P' ||
        !worker_master_is_clean(worker) ||
        !worker_directory_matches(worker, packet) ||
        read_file_identity(runtime_path, &current_runtime) < 0 ||
        read_file_identity(runtime_image_path, &current_image) < 0 ||
        !file_identity_equal(&current_runtime, &runtime_identity) ||
        !file_identity_equal(&current_image, &runtime_image_identity)) {
        worker_discard(worker);
        close(client);
        ensure_ready_worker(workers);
        return;
    }
    ready = (uint32_t)worker_count(workers, worker_ready);
    if (send_offer(client, worker->master_fd, ready, active) < 0 ||
        read_byte_with_timeout(client, &commit, 1500) < 0 || commit != 'C') {
        worker_discard(worker);
        close(client);
        ensure_ready_worker(workers);
        return;
    }
    if (write_all(worker->control_fd, "G", 1) < 0) {
        worker_discard(worker);
        close(client);
        ensure_ready_worker(workers);
        return;
    }
    close(worker->control_fd);
    worker->control_fd = -1;
    worker->client_fd = client;
    worker->state = worker_active;
    if (send_reply(client, reply_committed,
                   (uint32_t)worker_count(workers, worker_ready),
                   (uint32_t)worker_count(workers, worker_active)) < 0) {
        worker_discard(worker);
        ensure_ready_worker(workers);
        return;
    }
    ensure_ready_worker(workers);
}

static void
handle_client(int client, struct worker *workers)
{
    unsigned char *packet;
    ssize_t length;
    uint16_t type;
    uint32_t ready;
    uint32_t active;
    if (!peer_is_owner(client)) {
        close(client);
        return;
    }
    packet = malloc(request_header_size + request_body_limit);
    if (packet == NULL) {
        close(client);
        return;
    }
    length = receive_request(client, packet,
                             request_header_size + request_body_limit);
    if (length < 0 || !request_valid(packet, (size_t)length)) {
        send_reply(client, reply_bad_request, 0, 0);
        free(packet);
        close(client);
        return;
    }
    type = get_u16(packet, 6);
    ready = (uint32_t)worker_count(workers, worker_ready);
    active = (uint32_t)worker_count(workers, worker_active);
    if ((type == request_stop || type == request_status) &&
        ((size_t)length != request_header_size || get_u32(packet, 24) != 0 ||
         get_u32(packet, 28) != 0)) {
        send_reply(client, reply_bad_request, ready, active);
        free(packet);
        close(client);
        return;
    }
    if (type == request_stop) {
        send_reply(client, reply_stopped, ready, active);
        daemon_stop_requested = 1;
        free(packet);
        close(client);
        return;
    }
    if (type == request_status) {
        send_reply(client,
                   (packet_identity_equal(packet, 32, &runtime_identity) &&
                    packet_identity_equal(packet, 64,
                                          &runtime_image_identity) &&
                    packet_identity_equal(packet, 96, &broker_identity))
                       ? reply_status
                       : reply_mismatch,
                   ready, active);
        free(packet);
        close(client);
        return;
    }
    if (type == request_lease) {
        if (!peer_context_matches(client) ||
            !lease_environment_compatible(packet)) {
            send_reply(client, reply_unavailable, ready, active);
            free(packet);
            close(client);
            return;
        }
        handle_lease(client, workers, packet, (size_t)length);
        free(packet);
        return;
    }
    send_reply(client, reply_bad_request, ready, active);
    free(packet);
    close(client);
}

static int
create_listener(int *lock_descriptor)
{
    int listener;
    struct sockaddr_un address;
    struct stat status;
    mode_t old_mask = umask(0077);
    *lock_descriptor = open(lock_path,
                            O_RDWR | O_CREAT | O_CLOEXEC | O_NOFOLLOW, 0600);
    umask(old_mask);
    if (*lock_descriptor < 0 || fstat(*lock_descriptor, &status) < 0 ||
        !S_ISREG(status.st_mode) || status.st_uid != own_uid ||
        (status.st_mode & 0077) != 0 ||
        flock(*lock_descriptor, LOCK_EX | LOCK_NB) < 0) {
        if (*lock_descriptor >= 0) {
            close(*lock_descriptor);
        }
        return -1;
    }
    if (lstat(socket_path, &status) == 0) {
        if (!S_ISSOCK(status.st_mode) || status.st_uid != own_uid ||
            unlink(socket_path) < 0) {
            close(*lock_descriptor);
            return -1;
        }
    } else if (errno != ENOENT) {
        close(*lock_descriptor);
        return -1;
    }
    listener = socket(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0);
    if (listener < 0) {
        close(*lock_descriptor);
        return -1;
    }
    memset(&address, 0, sizeof(address));
    address.sun_family = AF_UNIX;
    if (snprintf(address.sun_path, sizeof(address.sun_path), "%s",
                 socket_path) >= (int)sizeof(address.sun_path)) {
        close(listener);
        close(*lock_descriptor);
        return -1;
    }
    old_mask = umask(0077);
    if (bind(listener, (struct sockaddr *)&address, sizeof(address)) < 0 ||
        chmod(socket_path, 0600) < 0 || listen(listener, 16) < 0 ||
        set_nonblocking(listener) < 0) {
        umask(old_mask);
        close(listener);
        close(*lock_descriptor);
        unlink(socket_path);
        return -1;
    }
    umask(old_mask);
    return listener;
}

static void
daemon_signal_handler(int signal_number)
{
    (void)signal_number;
    daemon_stop_requested = 1;
}

static void
daemon_child_handler(int signal_number)
{
    (void)signal_number;
}

static int
daemon_loop(int notify_descriptor)
{
    struct worker workers[max_workers];
    int listener;
    int lock_descriptor = -1;
    int notified = 0;
    int index;
    struct sigaction action;
    memset(workers, 0, sizeof(workers));
    for (index = 0; index < max_workers; index++) {
        workers[index].master_fd = -1;
        workers[index].control_fd = -1;
        workers[index].client_fd = -1;
    }
    listener = create_listener(&lock_descriptor);
    if (listener < 0) {
        if (notify_descriptor >= 0) {
            write_all(notify_descriptor, "E", 1);
        }
        return 1;
    }
    memset(&action, 0, sizeof(action));
    action.sa_handler = daemon_signal_handler;
    sigemptyset(&action.sa_mask);
    sigaction(SIGTERM, &action, NULL);
    sigaction(SIGINT, &action, NULL);
    sigaction(SIGHUP, &action, NULL);
    action.sa_handler = daemon_child_handler;
    sigaction(SIGCHLD, &action, NULL);
    signal(SIGPIPE, SIG_IGN);
    ensure_ready_worker(workers);
    while (!daemon_stop_requested) {
        struct pollfd items[1 + max_workers];
        int item_count = 1;
        int result;
        worker_reap(workers);
        worker_poll_starting(workers);
        ensure_ready_worker(workers);
        if (!notified && worker_count(workers, worker_ready) > 0) {
            if (notify_descriptor >= 0) {
                write_all(notify_descriptor, "R", 1);
                close(notify_descriptor);
                notify_descriptor = -1;
            }
            notified = 1;
        }
        items[0].fd = listener;
        items[0].events = POLLIN;
        items[0].revents = 0;
        for (index = 0; index < max_workers; index++) {
            if (workers[index].state == worker_active &&
                workers[index].client_fd >= 0) {
                items[item_count].fd = workers[index].client_fd;
                items[item_count].events = POLLHUP | POLLRDHUP;
                items[item_count].revents = 0;
                item_count++;
            }
        }
        result = poll(items, (nfds_t)item_count, 20);
        if (result < 0 && errno != EINTR) {
            daemon_stop_requested = 1;
            continue;
        }
        if (result > 0) {
            int item_index = 1;
            for (index = 0; index < max_workers; index++) {
                struct worker *worker = &workers[index];
                if (worker->state != worker_active || worker->client_fd < 0) {
                    continue;
                }
                if (items[item_index].revents &
                    (POLLHUP | POLLRDHUP | POLLERR | POLLNVAL)) {
                    worker_discard(worker);
                }
                item_index++;
            }
        }
        if (result > 0 && (items[0].revents & POLLIN)) {
            for (;;) {
                int client = accept4(listener, NULL, NULL, SOCK_CLOEXEC);
                if (client < 0) {
                    if (errno == EINTR) {
                        continue;
                    }
                    break;
                }
                if (set_socket_timeouts(client, 750) < 0) {
                    close(client);
                    continue;
                }
                handle_client(client, workers);
            }
        }
    }
    if (notify_descriptor >= 0) {
        write_all(notify_descriptor, "E", 1);
        close(notify_descriptor);
    }
    close(listener);
    for (index = 0; index < max_workers; index++) {
        if (workers[index].state != worker_unused) {
            worker_discard(&workers[index]);
        }
    }
    while (waitpid(-1, NULL, 0) > 0 || errno == EINTR) {
    }
    unlink(socket_path);
    close(lock_descriptor);
    return 0;
}

static int
send_management_request(uint16_t type, unsigned char *reply,
                        int require_runtime_identity)
{
    unsigned char request[request_header_size];
    int descriptor = connect_supervisor(750);
    int received_fd = -1;
    if (descriptor < 0) {
        return -1;
    }
    make_header(request, type, 0, 0, 0, 0, 0);
    if (send(descriptor, request, sizeof(request), MSG_NOSIGNAL) !=
            (ssize_t)sizeof(request) ||
        receive_reply(descriptor, reply, &received_fd) < 0) {
        if (received_fd >= 0) {
            close(received_fd);
        }
        close(descriptor);
        return -1;
    }
    if (received_fd >= 0) {
        close(received_fd);
    }
    close(descriptor);
    if (!validate_reply_envelope(reply) ||
        (require_runtime_identity && !validate_reply(reply))) {
        return -1;
    }
    return 0;
}

static int
send_legacy_stop_request(uint16_t version, size_t header_size,
                         size_t legacy_reply_size)
{
    unsigned char request[128];
    unsigned char reply[112];
    int descriptor = connect_supervisor(750);
    ssize_t count;
    if (descriptor < 0 || header_size > sizeof(request) ||
        legacy_reply_size > sizeof(reply)) {
        if (descriptor >= 0) {
            close(descriptor);
        }
        return -1;
    }
    memset(request, 0, header_size);
    put_u32(request, 0, request_magic);
    put_u16(request, 4, version);
    put_u16(request, 6, request_stop);
    put_u64(request, 32, runtime_identity.device);
    put_u64(request, 40, runtime_identity.inode);
    if (header_size >= 64) {
        put_u64(request, 48, broker_identity.device);
        put_u64(request, 56, broker_identity.inode);
    }
    if (send(descriptor, request, header_size, MSG_NOSIGNAL) !=
        (ssize_t)header_size) {
        close(descriptor);
        return -1;
    }
    count = recv(descriptor, reply, legacy_reply_size, MSG_TRUNC);
    close(descriptor);
    if (count != (ssize_t)legacy_reply_size ||
        get_u32(reply, 0) != request_magic || get_u16(reply, 4) != version ||
        get_u16(reply, 6) != reply_stopped) {
        return -1;
    }
    return 0;
}

static int
daemon_status(int print_status, int require_ready)
{
    unsigned char reply[reply_size];
    uint16_t code;
    uint32_t ready;
    uint32_t active;
    if (send_management_request(request_status, reply, 1) < 0) {
        return 1;
    }
    code = get_u16(reply, 6);
    ready = get_u32(reply, 8);
    active = get_u32(reply, 12);
    if (print_status) {
        printf("ready=%u active=%u\n", ready, active);
    }
    return code == reply_status && (!require_ready || ready > 0) ? 0 : 1;
}

static int daemon_stop(void);

static int
daemon_start(void)
{
    int notification[2];
    pid_t child;
    unsigned char value;
    struct pollfd item;
    int null_descriptor;
    if (daemon_status(0, 0) == 0) {
        int attempt;
        for (attempt = 0; attempt < 100; attempt++) {
            if (daemon_status(0, 1) == 0) {
                return 0;
            }
            usleep(20000);
        }
        return 1;
    }
    if (socket_metadata_safe() && daemon_stop() != 0) {
        return 1;
    }
    if (pipe2(notification, O_CLOEXEC) < 0) {
        return 1;
    }
    child = fork();
    if (child < 0) {
        close(notification[0]);
        close(notification[1]);
        return 1;
    }
    if (child == 0) {
        sigset_t empty_mask;
        close(notification[0]);
        sigemptyset(&empty_mask);
        sigprocmask(SIG_SETMASK, &empty_mask, NULL);
        if (setsid() < 0) {
            write_all(notification[1], "E", 1);
            _exit(1);
        }
        null_descriptor = open("/dev/null", O_RDWR);
        if (null_descriptor >= 0) {
            dup2(null_descriptor, STDIN_FILENO);
            dup2(null_descriptor, STDOUT_FILENO);
            dup2(null_descriptor, STDERR_FILENO);
            if (null_descriptor > STDERR_FILENO) {
                close(null_descriptor);
            }
        }
        if (chdir("/") < 0) {
            write_all(notification[1], "E", 1);
            _exit(1);
        }
        close_inherited_descriptors(notification[1]);
        _exit(daemon_loop(notification[1]));
    }
    close(notification[1]);
    item.fd = notification[0];
    item.events = POLLIN;
    item.revents = 0;
    if (poll(&item, 1, 5000) <= 0 ||
        read(notification[0], &value, 1) != 1 || value != 'R') {
        kill(child, SIGTERM);
        close(notification[0]);
        return 1;
    }
    close(notification[0]);
    return 0;
}

static int
daemon_stop(void)
{
    unsigned char reply[reply_size];
    int attempt;
    if (send_management_request(request_stop, reply, 0) < 0) {
        if (send_legacy_stop_request(3, 128, 112) < 0 &&
            send_legacy_stop_request(2, 64, 48) < 0 &&
            send_legacy_stop_request(1, 48, 32) < 0) {
            return 0;
        }
    } else if (get_u16(reply, 6) != reply_stopped) {
        return 1;
    }
    for (attempt = 0; attempt < 100; attempt++) {
        if (access(socket_path, F_OK) < 0 && errno == ENOENT) {
            return 0;
        }
        usleep(20000);
    }
    return 1;
}

static int
daemon_command(int argc, char **argv)
{
    const char *operation;
    if (argc != 3) {
        fprintf(stderr,
                "usage: cclsh-fast daemon start|stop|status|restart\n");
        return 2;
    }
    operation = argv[2];
    if (strcmp(operation, "start") == 0) {
        return daemon_start();
    }
    if (strcmp(operation, "stop") == 0) {
        return daemon_stop();
    }
    if (strcmp(operation, "status") == 0) {
        return daemon_status(1, 1);
    }
    if (strcmp(operation, "restart") == 0) {
        return daemon_stop() == 0 ? daemon_start() : 1;
    }
    fprintf(stderr, "cclsh-fast: unknown daemon operation: %s\n", operation);
    return 2;
}

static int
descriptors_are_same_tty(void)
{
    struct stat input;
    struct stat output;
    struct stat error;
    return isatty(STDIN_FILENO) && isatty(STDOUT_FILENO) &&
           isatty(STDERR_FILENO) && fstat(STDIN_FILENO, &input) == 0 &&
           fstat(STDOUT_FILENO, &output) == 0 &&
           fstat(STDERR_FILENO, &error) == 0 &&
           input.st_rdev == output.st_rdev && input.st_rdev == error.st_rdev &&
           input.st_ino == output.st_ino && input.st_ino == error.st_ino &&
           tcgetpgrp(STDIN_FILENO) == getpgrp();
}

static int
standard_tty_descriptor_flags_are_compatible(void)
{
    int descriptor;
    for (descriptor = STDIN_FILENO; descriptor <= STDERR_FILENO;
         descriptor++) {
        int flags = fcntl(descriptor, F_GETFL);
        if (flags < 0 || (flags & O_ACCMODE) != O_RDWR ||
            (flags & (O_APPEND | O_ASYNC | O_NONBLOCK)) != 0) {
            return 0;
        }
    }
    return 1;
}

static int
client_signal_context_is_standard(void)
{
    sigset_t mask;
    struct sigaction action;
    int signal_number;
    if (sigprocmask(SIG_SETMASK, NULL, &mask) < 0) {
        return 0;
    }
    for (signal_number = 1; signal_number < NSIG; signal_number++) {
        if (sigismember(&mask, signal_number) == 1) {
            return 0;
        }
        if (signal_number == SIGKILL || signal_number == SIGSTOP) {
            continue;
        }
        if (sigaction(signal_number, NULL, &action) < 0) {
            if (errno == EINVAL) {
                continue;
            }
            return 0;
        }
        if (action.sa_handler != SIG_DFL) {
            return 0;
        }
    }
    return 1;
}

static int
client_has_only_standard_inheritable_descriptors(void)
{
    DIR *directory = opendir("/proc/self/fd");
    struct dirent *entry;
    int directory_fd;
    if (directory == NULL) {
        return 0;
    }
    directory_fd = dirfd(directory);
    while ((entry = readdir(directory)) != NULL) {
        char *end = NULL;
        long descriptor;
        int flags;
        errno = 0;
        descriptor = strtol(entry->d_name, &end, 10);
        if (errno != 0 || end == entry->d_name || *end != '\0' ||
            descriptor <= STDERR_FILENO || descriptor == directory_fd) {
            continue;
        }
        flags = fcntl((int)descriptor, F_GETFD);
        if (flags >= 0 && !(flags & FD_CLOEXEC)) {
            closedir(directory);
            return 0;
        }
    }
    closedir(directory);
    return 1;
}

static int
client_tty_environment_is_proxy_safe(void)
{
    static const char *variables[] = {"GPG_TTY", "SSH_TTY"};
    size_t index;
    for (index = 0; index < sizeof(variables) / sizeof(variables[0]); index++) {
        const char *value = getenv(variables[index]);
        if (value != NULL && value[0] != '\0') {
            return 0;
        }
    }
    return 1;
}

static int
client_environment_has_unique_names(void)
{
    char **left;
    for (left = environ; *left != NULL; left++) {
        const char *left_equals = strchr(*left, '=');
        size_t left_length;
        char **right;
        if (left_equals == NULL || left_equals == *left) {
            return 0;
        }
        left_length = (size_t)(left_equals - *left);
        for (right = left + 1; *right != NULL; right++) {
            const char *right_equals = strchr(*right, '=');
            if (right_equals == NULL || right_equals == *right) {
                return 0;
            }
            if ((size_t)(right_equals - *right) == left_length &&
                memcmp(*left, *right, left_length) == 0) {
                return 0;
            }
        }
    }
    return 1;
}

static void
close_inherited_descriptors(int preserved_descriptor)
{
    DIR *directory = opendir("/proc/self/fd");
    struct dirent *entry;
    int directory_fd;
    if (directory == NULL) {
        long limit = sysconf(_SC_OPEN_MAX);
        int descriptor;
        if (limit < 0 || limit > 1048576) {
            limit = 1048576;
        }
        for (descriptor = STDERR_FILENO + 1; descriptor < limit;
             descriptor++) {
            if (descriptor != preserved_descriptor) {
                close(descriptor);
            }
        }
        return;
    }
    directory_fd = dirfd(directory);
    while ((entry = readdir(directory)) != NULL) {
        char *end = NULL;
        long descriptor;
        errno = 0;
        descriptor = strtol(entry->d_name, &end, 10);
        if (errno == 0 && end != entry->d_name && *end == '\0' &&
            descriptor > STDERR_FILENO && descriptor != directory_fd &&
            descriptor != preserved_descriptor) {
            close((int)descriptor);
        }
    }
    closedir(directory);
}

static unsigned char *
build_lease_request(size_t *packet_length)
{
    char *cwd = NULL;
    int cwd_descriptor = -1;
    struct stat cwd_status;
    struct stat path_status;
    size_t cwd_length;
    size_t shell_length = strlen(runtime_path);
    size_t environment_length = 0;
    size_t body_length;
    size_t offset;
    unsigned char *packet;
    char **entry;
    mode_t mask;
    cwd_descriptor = open(".", O_PATH | O_DIRECTORY | O_CLOEXEC);
    cwd = getcwd(NULL, 0);
    if (cwd_descriptor < 0 || fstat(cwd_descriptor, &cwd_status) < 0 ||
        cwd == NULL || cwd[0] != '/' || stat(cwd, &path_status) < 0 ||
        cwd_status.st_dev != path_status.st_dev ||
        cwd_status.st_ino != path_status.st_ino) {
        if (cwd_descriptor >= 0) {
            close(cwd_descriptor);
        }
        free(cwd);
        return NULL;
    }
    close(cwd_descriptor);
    cwd_length = strlen(cwd);
    for (entry = environ; *entry != NULL; entry++) {
        size_t length;
        if (strncmp(*entry, "CCLSH_FAST_WORKER_FD=", 21) == 0) {
            continue;
        }
        length = strlen(*entry) + 1;
        if (length > request_body_limit - environment_length) {
            free(cwd);
            return NULL;
        }
        environment_length += length;
    }
    if (cwd_length == 0 || shell_length == 0 ||
        cwd_length > request_body_limit || shell_length > request_body_limit ||
        cwd_length > request_body_limit - shell_length ||
        cwd_length + shell_length >
            request_body_limit - environment_length) {
        free(cwd);
        return NULL;
    }
    body_length = cwd_length + shell_length + environment_length;
    packet = malloc(request_header_size + body_length);
    if (packet == NULL) {
        free(cwd);
        return NULL;
    }
    mask = umask(0);
    umask(mask);
    make_header(packet, request_lease, (uint32_t)body_length,
                (uint32_t)cwd_length, (uint32_t)shell_length,
                (uint32_t)environment_length, (uint32_t)(mask & 0777));
    put_u64(packet, 128, (uint64_t)cwd_status.st_dev);
    put_u64(packet, 136, (uint64_t)cwd_status.st_ino);
    offset = request_header_size;
    memcpy(packet + offset, cwd, cwd_length);
    offset += cwd_length;
    memcpy(packet + offset, runtime_path, shell_length);
    offset += shell_length;
    for (entry = environ; *entry != NULL; entry++) {
        size_t length;
        if (strncmp(*entry, "CCLSH_FAST_WORKER_FD=", 21) == 0) {
            continue;
        }
        length = strlen(*entry) + 1;
        memcpy(packet + offset, *entry, length);
        offset += length;
    }
    free(cwd);
    *packet_length = request_header_size + body_length;
    return packet;
}

static void
relay_handler(int signal_number)
{
    if (signal_number == SIGWINCH) {
        relay_resize = 1;
    } else {
        relay_signal = signal_number;
    }
}

static int
install_relay_handlers(void)
{
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_handler = relay_handler;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGWINCH, &action, NULL) < 0 ||
        sigaction(SIGHUP, &action, NULL) < 0 ||
        sigaction(SIGTERM, &action, NULL) < 0 ||
        sigaction(SIGINT, &action, NULL) < 0 ||
        sigaction(SIGQUIT, &action, NULL) < 0 ||
        signal(SIGPIPE, SIG_IGN) == SIG_ERR) {
        reset_relay_handlers();
        return -1;
    }
    return 0;
}

static void
reset_relay_handlers(void)
{
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    sigaction(SIGWINCH, &action, NULL);
    sigaction(SIGHUP, &action, NULL);
    sigaction(SIGTERM, &action, NULL);
    sigaction(SIGINT, &action, NULL);
    sigaction(SIGQUIT, &action, NULL);
    sigaction(SIGPIPE, &action, NULL);
}

static int
set_terminal_attributes(int terminal_fd, const struct termios *attributes)
{
    sigset_t blocked;
    sigset_t previous;
    int result;
    int saved_errno;
    sigemptyset(&blocked);
    sigaddset(&blocked, SIGTTOU);
    if (sigprocmask(SIG_BLOCK, &blocked, &previous) < 0) {
        return -1;
    }
    do {
        result = tcsetattr(terminal_fd, TCSANOW, attributes);
    } while (result < 0 && errno == EINTR);
    saved_errno = errno;
    if (sigprocmask(SIG_SETMASK, &previous, NULL) < 0 && result == 0) {
        return -1;
    }
    errno = saved_errno;
    return result;
}

static int
relay_session(int terminal_fd, int master_fd, int status_fd)
{
    enum { relay_capacity = 65536 };
    struct relay_buffer {
        unsigned char data[relay_capacity];
        size_t start;
        size_t length;
    } input = {{0}, 0, 0}, output = {{0}, 0, 0};
    int master_read_eof = 0;
    int master_write_closed = 0;
    int master_hup = 0;
    int status_closed = 0;
    int status_received = 0;
    int relay_incomplete = 0;
    int exit_status = 70;
    int64_t drain_deadline = -1;
    while (!relay_signal) {
        struct pollfd items[4];
        int result;
        int timeout = 100;
        if ((status_received || status_closed) && master_read_eof &&
            output.length == 0) {
            break;
        }
        if (master_read_eof && drain_deadline < 0) {
            drain_deadline = monotonic_milliseconds() + 500;
        }
        if (drain_deadline >= 0) {
            int64_t remaining = drain_deadline - monotonic_milliseconds();
            if (remaining <= 0) {
                relay_incomplete = 1;
                break;
            }
            if (remaining < timeout) {
                timeout = (int)remaining;
            }
        }
        items[0].fd = terminal_fd;
        items[0].events = (!master_write_closed && !status_received &&
                           !status_closed &&
                           input.length < relay_capacity)
                              ? POLLIN
                              : 0;
        items[0].revents = 0;
        items[1].fd = (master_read_eof && master_write_closed) ? -1 : master_fd;
        items[1].events = 0;
        if (!master_read_eof && output.length < relay_capacity) {
            items[1].events |= POLLIN;
        }
        if (!master_write_closed && input.length > 0) {
            items[1].events |= POLLOUT;
        }
        if (items[1].events == 0) {
            items[1].fd = -1;
        }
        items[1].revents = 0;
        items[2].fd = output.length > 0 ? terminal_fd : -1;
        items[2].events = output.length > 0 ? POLLOUT : 0;
        items[2].revents = 0;
        items[3].fd = status_closed ? -1 : status_fd;
        items[3].events = status_closed ? 0 : POLLIN;
        items[3].revents = 0;
        result = poll(items, 4, timeout);
        if (result < 0 && errno != EINTR) {
            break;
        }
        if (relay_resize) {
            struct winsize size;
            relay_resize = 0;
            if (ioctl(terminal_fd, TIOCGWINSZ, &size) == 0) {
                ioctl(master_fd, TIOCSWINSZ, &size);
            }
        }
        if (result <= 0) {
            continue;
        }
        if (items[0].revents & POLLIN) {
            ssize_t count;
            if (input.start + input.length == relay_capacity &&
                input.start > 0) {
                memmove(input.data, input.data + input.start, input.length);
                input.start = 0;
            }
            count = read(terminal_fd,
                         input.data + input.start + input.length,
                         relay_capacity - input.start - input.length);
            if (count > 0) {
                input.length += (size_t)count;
            } else if (count == 0 ||
                       (count < 0 && errno != EINTR && errno != EAGAIN &&
                        errno != EWOULDBLOCK)) {
                break;
            }
        }
        if (items[0].revents & (POLLHUP | POLLERR | POLLNVAL)) {
            break;
        }
        if (items[2].revents & POLLOUT) {
            ssize_t count = write(terminal_fd, output.data + output.start,
                                  output.length);
            if (count > 0) {
                output.start += (size_t)count;
                output.length -= (size_t)count;
                if (output.length == 0) {
                    output.start = 0;
                }
            } else if (count < 0 && errno != EINTR && errno != EAGAIN &&
                       errno != EWOULDBLOCK) {
                break;
            }
        }
        if (items[2].revents & (POLLHUP | POLLERR | POLLNVAL)) {
            break;
        }
        if (items[1].revents & (POLLHUP | POLLERR | POLLNVAL)) {
            master_hup = 1;
            master_write_closed = 1;
            input.start = 0;
            input.length = 0;
        }
        if ((items[1].revents & POLLIN) || master_hup) {
            for (;;) {
                ssize_t count;
                if (output.start + output.length == relay_capacity &&
                    output.start > 0) {
                    memmove(output.data, output.data + output.start,
                            output.length);
                    output.start = 0;
                }
                if (output.start + output.length == relay_capacity) {
                    break;
                }
                count = read(master_fd,
                             output.data + output.start + output.length,
                             relay_capacity - output.start - output.length);
                if (count > 0) {
                    output.length += (size_t)count;
                    if (master_hup) {
                        continue;
                    }
                } else if (count == 0 || (count < 0 && errno == EIO)) {
                    master_read_eof = 1;
                } else if (count < 0 && errno == EINTR) {
                    continue;
                } else if (count < 0 &&
                           (errno == EAGAIN || errno == EWOULDBLOCK)) {
                    if (master_hup) {
                        master_read_eof = 1;
                    }
                } else if (count < 0) {
                    master_read_eof = 1;
                }
                break;
            }
        }
        if ((items[1].revents & POLLOUT) && input.length > 0) {
            ssize_t count = write(master_fd, input.data + input.start,
                                  input.length);
            if (count > 0) {
                input.start += (size_t)count;
                input.length -= (size_t)count;
                if (input.length == 0) {
                    input.start = 0;
                }
            } else if (count < 0 && errno != EINTR && errno != EAGAIN &&
                       errno != EWOULDBLOCK) {
                master_write_closed = 1;
                input.start = 0;
                input.length = 0;
            }
        }
        if (items[3].revents & POLLIN) {
            unsigned char encoded[4];
            ssize_t count = recv(status_fd, encoded, sizeof(encoded), 0);
            if (count == 4) {
                exit_status = (int)get_u32(encoded, 0);
                status_received = 1;
                input.start = 0;
                input.length = 0;
                drain_deadline = monotonic_milliseconds() + 500;
            } else if (count == 0) {
                status_closed = 1;
                input.start = 0;
                input.length = 0;
                drain_deadline = monotonic_milliseconds() + 500;
            }
        }
        if (items[3].revents & (POLLHUP | POLLERR | POLLNVAL)) {
            status_closed = 1;
            input.start = 0;
            input.length = 0;
            if (drain_deadline < 0) {
                drain_deadline = monotonic_milliseconds() + 500;
            }
        }
    }
    if (relay_signal) {
        return 128 + relay_signal;
    }
    if (relay_incomplete) {
        return 70;
    }
    return status_received ? exit_status : 70;
}

static int
try_fast_session(void)
{
    int descriptor = -1;
    int master_fd = -1;
    int confirmation_fd = -1;
    int terminal_fd = -1;
    unsigned char *request = NULL;
    size_t request_length = 0;
    unsigned char reply[reply_size];
    struct termios saved;
    struct termios raw;
    struct winsize size;
    int committed = 0;
    int terminal_raw = 0;
    int master_flags = -1;
    int terminal_flags = -1;
    int result = -1;
    terminal_fd = open("/dev/tty", O_RDWR | O_CLOEXEC | O_NOCTTY);
    if (terminal_fd < 0 || tcgetpgrp(terminal_fd) != getpgrp()) {
        goto cleanup;
    }
    descriptor = connect_supervisor(40);
    if (descriptor < 0) {
        goto cleanup;
    }
    request = build_lease_request(&request_length);
    if (request == NULL ||
        send(descriptor, request, request_length, MSG_NOSIGNAL) !=
            (ssize_t)request_length ||
        receive_reply(descriptor, reply, &master_fd) < 0 ||
        !validate_reply(reply) || get_u16(reply, 6) != reply_offer ||
        master_fd < 0 || tcgetattr(terminal_fd, &saved) < 0 ||
        set_terminal_attributes(master_fd, &saved) < 0 ||
        ioctl(terminal_fd, TIOCGWINSZ, &size) < 0 ||
        ioctl(master_fd, TIOCSWINSZ, &size) < 0 ||
        (master_flags = fcntl(master_fd, F_GETFL)) < 0 ||
        (terminal_flags = fcntl(terminal_fd, F_GETFL)) < 0 ||
        set_nonblocking(master_fd) < 0 ||
        set_nonblocking(terminal_fd) < 0) {
        goto cleanup;
    }
    raw = saved;
    cfmakeraw(&raw);
    relay_signal = 0;
    relay_resize = 0;
    if (install_relay_handlers() < 0) {
        goto cleanup;
    }
    if (set_terminal_attributes(terminal_fd, &raw) < 0) {
        goto cleanup;
    }
    terminal_raw = 1;
    if (relay_signal) {
        result = 128 + relay_signal;
        goto cleanup;
    }
    {
        ssize_t sent;
        do {
            sent = send(descriptor, "C", 1, MSG_NOSIGNAL);
        } while (sent < 0 && errno == EINTR && !relay_signal);
        if (relay_signal) {
            result = 128 + relay_signal;
            goto cleanup;
        }
        if (sent != 1) {
            goto cleanup;
        }
    }
    committed = 1;
    if (receive_reply(descriptor, reply, &confirmation_fd) < 0 ||
        confirmation_fd >= 0 || !validate_reply(reply) ||
        get_u16(reply, 6) != reply_committed) {
        result = 70;
        goto cleanup;
    }
    result = relay_session(terminal_fd, master_fd, descriptor);

cleanup:
    {
        int descriptor_restore_error = 0;
        if (master_flags >= 0 &&
            fcntl(master_fd, F_SETFL, master_flags) < 0) {
            descriptor_restore_error = errno;
        }
        if (terminal_flags >= 0 &&
            fcntl(terminal_fd, F_SETFL, terminal_flags) < 0 &&
            descriptor_restore_error == 0) {
            descriptor_restore_error = errno;
        }
        if (descriptor_restore_error != 0) {
            dprintf(STDERR_FILENO,
                    "cclsh-fast: could not restore descriptor flags: %s\n",
                    strerror(descriptor_restore_error));
            result = 70;
            committed = 1;
        }
    }
    if (terminal_raw) {
        if (set_terminal_attributes(terminal_fd, &saved) < 0) {
            dprintf(STDERR_FILENO,
                    "cclsh-fast: could not restore terminal: %s\n",
                    strerror(errno));
            result = 70;
            committed = 1;
        }
    }
    free(request);
    if (confirmation_fd >= 0) {
        close(confirmation_fd);
    }
    if (master_fd >= 0) {
        close(master_fd);
    }
    if (descriptor >= 0) {
        close(descriptor);
    }
    if (terminal_fd >= 0) {
        close(terminal_fd);
    }
    reset_relay_handlers();
    if (relay_signal) {
        result = 128 + relay_signal;
    }
    return committed || relay_signal ? result : -1;
}

int
main(int argc, char **argv)
{
    int result;
    if (initialize_runtime_path() < 0) {
        fprintf(stderr, "cclsh-fast: sibling cclsh runtime unavailable\n");
        return 127;
    }
    if (argc >= 2 && strcmp(argv[1], "daemon") == 0) {
        if (initialize_daemon_paths() < 0) {
            fprintf(stderr,
                    "cclsh-fast: secure XDG runtime directory unavailable\n");
            return 1;
        }
        return daemon_command(argc, argv);
    }
    if (argc != 1 || argv[0] == NULL || argv[0][0] == '-' ||
        getenv("CCLSH_FAST_WORKER_FD") != NULL ||
        !descriptors_are_same_tty() ||
        !standard_tty_descriptor_flags_are_compatible() ||
        !client_signal_context_is_standard() ||
        !client_tty_environment_is_proxy_safe() ||
        !client_environment_has_unique_names() ||
        !client_has_only_standard_inheritable_descriptors()) {
        direct_exec(argc, argv);
    }
    if (initialize_daemon_paths() < 0) {
        direct_exec(argc, argv);
    }
    result = try_fast_session();
    if (result < 0) {
        direct_exec(argc, argv);
    }
    return result;
}
