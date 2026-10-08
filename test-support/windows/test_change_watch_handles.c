/* Independent OS-resource oracle for repeated native provider teardown. */
#include "fx_win32.h"
#include <stdio.h>
void *fo_change_native_open(int *);
int fo_change_native_root(void *, const char *);
int fo_change_native_reconcile(void *);
void fo_change_native_close(void *);
int wp_setup(char *, int);
void wp_cleanup(const char *);
int main(void) {
    char root[4096];
    DWORD before, after;
    int i, error;
    void *watch;
    if (!wp_setup(root, sizeof(root))) return 1;
    for (i = 0; i < 51; ++i) {
        watch = fo_change_native_open(&error);
        if (!watch || error || fo_change_native_root(watch, root) ||
            fo_change_native_reconcile(watch)) {
            fo_change_native_close(watch); wp_cleanup(root); return 1;
        }
        fo_change_native_close(watch);
        if (!i) {
            puts("WARM native provider and console initialized");
            if (!GetProcessHandleCount(GetCurrentProcess(), &before)) return 1;
        }
    }
    if (!GetProcessHandleCount(GetCurrentProcess(), &after)) return 1;
    wp_cleanup(root);
    printf("%s 50 subscribe/cancel/close cycles: handles before=%lu after=%lu\n",
           before == after ? "PASS" : "FAIL", (unsigned long)before, (unsigned long)after);
    return before == after ? 0 : 1;
}
