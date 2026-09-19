#ifndef CCLSH_SESSION_IPC_H
#define CCLSH_SESSION_IPC_H

#include <stdint.h>

/* Same-host protocol: native uint32 fields, UTF-8 NUL-terminated strings,
 * and SCM_RIGHTS descriptors. It never transmits readable Lisp objects. */
enum {
    sh_hello = 1, sh_accept, sh_status, sh_stop, sh_spawn, sh_reply,
    sh_child, sh_foreground, sh_kill, sh_exit, sh_open, sh_monitor,
    sh_capacity = 131072, sh_descriptor_limit = 8
};

int sh_send(int socket, uint32_t type, uint32_t token, const void *body,
            uint32_t length, const int *descriptors, uint32_t count);
int sh_receive(int socket, uint32_t *type, uint32_t *token, void *body,
               uint32_t *length, int *descriptors, uint32_t *count);
int sh_listen(const char *path);
int sh_accept_peer(int listener);
int sh_connect(const char *path);
int sh_shutdown(int descriptor);

#endif
