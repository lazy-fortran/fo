/* A process-local UTF-8 code page preserves the UTF-16 Windows command line
   in the UCRT argv consumed by Fortran get_command_argument. GCC emits this
   RT_MANIFEST resource in the already-retained driver object; no global locale
   change or separate resource build step is required. Windows 10 1903+ applies
   activeCodePage; older Windows is outside this native Unicode contract. */
#ifndef FO_DRIVER_UTF8_H
#define FO_DRIVER_UTF8_H
#ifdef _WIN32
__asm__(
    ".section .rsrc,\"dr\"\n"
    ".balign 4\n"
    "fo_utf8_rsrc:\n"
    ".long 0,0\n"
    ".short 0,0,0,1\n"
    ".long 24,0x80000000+(fo_utf8_type-fo_utf8_rsrc)\n"
    "fo_utf8_type:\n"
    ".long 0,0\n"
    ".short 0,0,0,1\n"
    ".long 1,0x80000000+(fo_utf8_name-fo_utf8_rsrc)\n"
    "fo_utf8_name:\n"
    ".long 0,0\n"
    ".short 0,0,0,1\n"
    ".long 0x409,fo_utf8_entry-fo_utf8_rsrc\n"
    "fo_utf8_entry:\n"
    ".rva fo_utf8_data\n"
    ".long fo_utf8_end-fo_utf8_data,65001,0\n"
    "fo_utf8_data:\n"
    ".ascii \"<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\" standalone=\\\"yes\\\"?><assembly xmlns=\\\"urn:schemas-microsoft-com:asm.v1\\\" manifestVersion=\\\"1.0\\\"><assemblyIdentity version=\\\"1.0.0.0\\\" name=\\\"LazyFortran.Fo\\\" type=\\\"win32\\\"/><application xmlns=\\\"urn:schemas-microsoft-com:asm.v3\\\"><windowsSettings><activeCodePage xmlns=\\\"http://schemas.microsoft.com/SMI/2019/WindowsSettings\\\">UTF-8</activeCodePage></windowsSettings></application></assembly>\"\n"
    "fo_utf8_end:\n"
    ".text\n"
);
#endif
#endif
