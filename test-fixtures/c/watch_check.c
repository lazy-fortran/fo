/* Independent recorder of actual fo watch check invocations. */
#include <stdio.h>
#include <stdlib.h>
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <wchar.h>
int wmain(void) {
    const wchar_t *path = _wgetenv(L"FO_WATCH_CHECK_MARKER");
    FILE *stream = path ? _wfopen(path, L"a") : NULL;
#else
int main(void) {
    const char *path = getenv("FO_WATCH_CHECK_MARKER");
    FILE *stream = path ? fopen(path, "a") : NULL;
#endif
    if (!stream) return 1;
    fputs("check\n", stream);
    return fclose(stream) ? 2 : 0;
}
