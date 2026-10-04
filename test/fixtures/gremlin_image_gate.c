/* Test-only Darwin OS interposition: hold an executing owned image before pin. */
#ifdef __APPLE__
#define _DARWIN_C_SOURCE 1
#include <mach-o/dyld.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

static int image_path_gate(char *path, uint32_t *capacity) {
    /* dyld exempts calls from the interposing image itself. */
    int result = _NSGetExecutablePath(path, capacity);
    const char *gate = getenv("FO_DRIVER_PIN_GATE");
    if (result == 0 && gate) {
        char log[4096];
        snprintf(log, sizeof(log), "%s.log", gate);
        int fd = open(log, O_WRONLY | O_CREAT | O_APPEND, 0600);
        if (fd >= 0) {
            (void)dup2(fd, STDOUT_FILENO);
            (void)dup2(fd, STDERR_FILENO);
            dprintf(fd, "running image: %s\n", path);
            close(fd);
        }
    }
    if (result == 0 && gate && strstr(path, "/fo-gremlin-images-")) {
        char marker[4096], byte;
        snprintf(marker, sizeof(marker), "%s.started", gate);
        int fd = open(marker, O_WRONLY | O_CREAT | O_EXCL, 0600);
        if (fd >= 0) {
            close(fd);
            fd = open(gate, O_RDONLY);
            if (fd >= 0) { (void)read(fd, &byte, 1); close(fd); }
        }
        unsetenv("FO_DRIVER_PIN_GATE");
    }
    return result;
}

__attribute__((used)) static struct {
    const void *replacement;
    const void *original;
} image_path_interposition __attribute__((section("__DATA,__interpose"))) = {
    (const void *)image_path_gate, (const void *)_NSGetExecutablePath
};
#else
int gremlin_image_gate_darwin_only(void) { return 0; }
#endif
