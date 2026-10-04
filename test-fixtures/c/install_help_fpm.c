/* Tiny fpm stand-in used by the Fortran CLI oracle. It records every
 * build/install subprocess and, only when enabled, copies the expected fo
 * candidate into the requested isolated prefix. */
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

static int copy_file(const char *source, const char *target) {
    FILE *input = fopen(source, "rb");
    if (input == NULL) return 1;
    FILE *output = fopen(target, "wb");
    if (output == NULL) { fclose(input); return 2; }
    unsigned char buffer[16384];
    size_t size;
    int result = 0;
    while ((size = fread(buffer, 1, sizeof buffer, input)) > 0)
        if (fwrite(buffer, 1, size, output) != size) { result = 3; break; }
    if (ferror(input)) result = 4;
    if (fclose(input) != 0 || fclose(output) != 0) result = 5;
    if (result == 0 && chmod(target, 0755) != 0) result = 6;
    return result;
}

int main(int argc, char **argv) {
    const char *marker = getenv("FO_INSTALL_HELP_MARKER");
    FILE *log = marker == NULL ? NULL : fopen(marker, "wb");
    if (log == NULL) return 20;
    for (int i = 0; i < argc; ++i) fprintf(log, "%s\n", argv[i]);
    if (fclose(log) != 0) return 21;

    if (getenv("FO_INSTALL_HELP_DO_INSTALL") == NULL) return 0;
    const char *prefix = getenv("FO_INSTALL_HELP_PREFIX");
    const char *expected = getenv("FO_INSTALL_HELP_EXPECTED");
    if (prefix == NULL || expected == NULL || argc != 6 ||
        strcmp(argv[1], "install") != 0 || strcmp(argv[2], "--profile") != 0 ||
        strcmp(argv[3], "release") != 0 || strcmp(argv[4], "--prefix") != 0)
        return 22;
    if (strcmp(argv[5], prefix) != 0) return 23;
    char target[4096];
    if (snprintf(target, sizeof target, "%s/bin/fo", prefix) >= (int)sizeof target)
        return 24;
    return copy_file(expected, target);
}
