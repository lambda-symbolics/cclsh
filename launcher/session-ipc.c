#define _GNU_SOURCE
#include "session-ipc.h"
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/un.h>
#include <unistd.h>

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

static int server_lock = -1;

static int peer_is_owner(int descriptor)
{
#ifdef __linux__
    struct ucred peer;
    socklen_t size = sizeof(peer);
    return getsockopt(descriptor, SOL_SOCKET, SO_PEERCRED, &peer, &size) == 0 &&
           peer.uid == geteuid();
#else
    uid_t uid;
    gid_t gid;
    return getpeereid(descriptor, &uid, &gid) == 0 && uid == geteuid();
#endif
}

int sh_send(int socket, uint32_t type, uint32_t token, const void *body,
            uint32_t length, const int *descriptors, uint32_t count)
{
    uint32_t header[] = {0x53484c31, 1, type, token, length};
    struct iovec vectors[] = {{header, sizeof(header)}, {(void *)body, length}};
    union { struct cmsghdr align; char bytes[CMSG_SPACE(8 * sizeof(int))]; } control;
    struct msghdr message = {0};
    ssize_t result;
    if (length > sh_capacity || count > sh_descriptor_limit) return -EMSGSIZE;
    message.msg_iov = vectors;
    message.msg_iovlen = 2;
    if (count) {
        struct cmsghdr *item;
        memset(&control, 0, sizeof(control));
        message.msg_control = control.bytes;
        message.msg_controllen = CMSG_SPACE(count * sizeof(int));
        item = CMSG_FIRSTHDR(&message);
        item->cmsg_level = SOL_SOCKET;
        item->cmsg_type = SCM_RIGHTS;
        item->cmsg_len = CMSG_LEN(count * sizeof(int));
        memcpy(CMSG_DATA(item), descriptors, count * sizeof(int));
    }
    do { result = sendmsg(socket, &message, MSG_NOSIGNAL); }
    while (result < 0 && errno == EINTR);
    return result == (ssize_t)(sizeof(header) + length) ? 0 :
           -(result < 0 ? errno : EIO);
}

int sh_receive(int socket, uint32_t *type, uint32_t *token, void *body,
               uint32_t *length, int *descriptors, uint32_t *count)
{
    uint32_t header[5];
    struct iovec vectors[] = {{header, sizeof(header)}, {body, sh_capacity}};
    union { struct cmsghdr align; char bytes[CMSG_SPACE(8 * sizeof(int))]; } control;
    struct msghdr message = {0};
    struct cmsghdr *item;
    ssize_t result;
    int error = 0;
    *count = 0;
    message.msg_iov = vectors;
    message.msg_iovlen = 2;
    message.msg_control = control.bytes;
    message.msg_controllen = sizeof(control);
    do { result = recvmsg(socket, &message, 0); }
    while (result < 0 && errno == EINTR);
    if (result <= 0) return -(result < 0 ? errno : ECONNRESET);
    for (item = CMSG_FIRSTHDR(&message); item;
         item = CMSG_NXTHDR(&message, item)) {
        if (item->cmsg_level == SOL_SOCKET && item->cmsg_type == SCM_RIGHTS) {
            size_t index, n = (item->cmsg_len - CMSG_LEN(0)) / sizeof(int);
            int *received = (int *)CMSG_DATA(item);
            for (index = 0; index < n; index++) {
                if (*count == sh_descriptor_limit) {
                    close(received[index]);
                    error = EMSGSIZE;
                } else {
                    descriptors[(*count)++] = received[index];
                    if (fcntl(received[index], F_SETFD, FD_CLOEXEC) < 0)
                        error = errno;
                }
            }
        }
    }
    if ((message.msg_flags & (MSG_TRUNC | MSG_CTRUNC)) ||
        result < (ssize_t)sizeof(header) || header[0] != 0x53484c31 ||
        header[1] != 1 || header[4] > sh_capacity ||
        result != (ssize_t)(sizeof(header) + header[4])) error = EPROTO;
    if (error) {
        while (*count) close(descriptors[--*count]);
        return -error;
    }
    *type = header[2]; *token = header[3]; *length = header[4];
    return 0;
}

static int address_for(const char *path, struct sockaddr_un *address)
{
    if (path[0] != '/' || strlen(path) >= sizeof(address->sun_path))
        return -ENAMETOOLONG;
    memset(address, 0, sizeof(*address));
    address->sun_family = AF_UNIX;
    strcpy(address->sun_path, path);
    return 0;
}

int sh_listen(const char *path)
{
    struct sockaddr_un address;
    struct stat status;
    char directory[sizeof(address.sun_path)], lock_path[sizeof(address.sun_path) + 6];
    char *slash;
    int descriptor, error = address_for(path, &address);
    mode_t mask;
    if (error) return error;
    strcpy(directory, path);
    slash = strrchr(directory, '/');
    if (!slash || slash == directory) return -EINVAL;
    *slash = '\0';
    if (lstat(directory, &status) < 0) return -errno;
    if (!S_ISDIR(status.st_mode) || status.st_uid != geteuid() ||
        (status.st_mode & 077) != 0) return -EACCES;
    snprintf(lock_path, sizeof(lock_path), "%s.lock", path);
    server_lock = open(lock_path, O_CREAT | O_RDWR | O_NOFOLLOW | O_CLOEXEC, 0600);
    if (server_lock < 0) return -errno;
    if (flock(server_lock, LOCK_EX | LOCK_NB) < 0) {
        error = errno; close(server_lock); server_lock = -1; return -error;
    }
    descriptor = socket(AF_UNIX, SOCK_SEQPACKET, 0);
    if (descriptor < 0) { error = errno; goto failure; }
    fcntl(descriptor, F_SETFD, FD_CLOEXEC);
    if (lstat(path, &status) == 0 &&
        (!S_ISSOCK(status.st_mode) || status.st_uid != geteuid())) {
        error = EACCES; goto failure;
    }
    unlink(path);
    mask = umask(077);
    error = bind(descriptor, (struct sockaddr *)&address, sizeof(address));
    umask(mask);
    if (error < 0 || listen(descriptor, 32) < 0) { error = errno; goto failure; }
    return descriptor;
failure:
    if (descriptor >= 0) close(descriptor);
    close(server_lock); server_lock = -1;
    return -error;
}

int sh_accept_peer(int listener)
{
    int descriptor;
    do { descriptor = accept(listener, NULL, NULL); }
    while (descriptor < 0 && errno == EINTR);
    if (descriptor < 0) return -errno;
    if (!peer_is_owner(descriptor)) { close(descriptor); return -EACCES; }
    fcntl(descriptor, F_SETFD, FD_CLOEXEC);
    return descriptor;
}

int sh_connect(const char *path)
{
    struct sockaddr_un address;
    struct stat status;
    struct timeval timeout = {5, 0};
    int descriptor, error = address_for(path, &address);
    if (error) return error;
    if (lstat(path, &status) < 0) return -errno;
    if (!S_ISSOCK(status.st_mode) || status.st_uid != geteuid() ||
        (status.st_mode & 077)) return -EACCES;
    descriptor = socket(AF_UNIX, SOCK_SEQPACKET, 0);
    if (descriptor < 0) return -errno;
    setsockopt(descriptor, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    setsockopt(descriptor, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));
    if (connect(descriptor, (struct sockaddr *)&address, sizeof(address)) < 0 ||
        !peer_is_owner(descriptor)) {
        error = errno ? errno : EACCES; close(descriptor); return -error;
    }
    fcntl(descriptor, F_SETFD, FD_CLOEXEC);
    return descriptor;
}

int sh_shutdown(int descriptor)
{
    return shutdown(descriptor, SHUT_RDWR);
}
