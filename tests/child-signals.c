#define _POSIX_C_SOURCE 200809L
#include <signal.h>
#include <stdio.h>

int main(void)
{
    const int signals[] = {SIGTTIN, SIGTTOU, SIGTSTP};
    unsigned int index;
    for (index = 0; index < sizeof(signals) / sizeof(signals[0]); index++) {
        struct sigaction action;
        if (sigaction(signals[index], NULL, &action) < 0 ||
            action.sa_handler != SIG_DFL) {
            fprintf(stderr, "child inherited non-default signal %d\n",
                    signals[index]);
            return 1;
        }
    }
    puts("default-child-signals");
    return 0;
}
