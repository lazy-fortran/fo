#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "fx_win_store.h"
int fo_c_driver_image_init(void);
int fo_c_driver_stage_copy(const char *, char *, int);
int fo_c_driver_publish(const char *,const char *,const char *,char *,int);
int fo_c_driver_remove_stage(const char *,const char *);
int fo_c_driver_validate_root(const char *);
int fo_c_driver_validate_pin(const char *,long long *);
static int errors;
static void check(int yes,const char *label){printf("%s: %s errno=%d winerror=%lu\n",yes?"PASS":"FAIL",label,errno,GetLastError());if(!yes)++errors;}
static wchar_t *wide(const char *s){int n=MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,s,-1,NULL,0);wchar_t *w=calloc((size_t)n,sizeof(*w));if(!w||!MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,s,-1,w,n)){free(w);return NULL;}return w;}
static int same_file_bytes(const wchar_t *a,const wchar_t *b){HANDLE x=CreateFileW(a,GENERIC_READ,FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,NULL,OPEN_EXISTING,0,NULL),y=CreateFileW(b,GENERIC_READ,FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,NULL,OPEN_EXISTING,0,NULL);int same=0;if(x!=INVALID_HANDLE_VALUE&&y!=INVALID_HANDLE_VALUE){char p[4096],q[4096];DWORD n,m;for(;;){if(!ReadFile(x,p,sizeof(p),&n,NULL)||!ReadFile(y,q,sizeof(q),&m,NULL)||n!=m||memcmp(p,q,n))break;if(!n){same=1;break;}}}if(x!=INVALID_HANDLE_VALUE)CloseHandle(x);if(y!=INVALID_HANDLE_VALUE)CloseHandle(y);return same;}
int main(int argc,char **argv){
 if(argc>1&&!strcmp(argv[1],"--pinned-child"))return 17;
 if(argc!=2)return 2;
 const char *root=argv[1],*digest="0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
 char stage[4096],pin[4096],stage2[4096],pin2[4096],bad[4096];wchar_t image[32768];long long size=0;
 check(GetModuleFileNameW(NULL,image,32768)>0,"independent running PE pathname");
 check(fo_c_driver_image_init()==0,"retains native running image without Unix execute bits");
 check(fo_c_driver_stage_copy(root,stage,sizeof(stage))==0,"copies retained native image into private readonly staging");
 wchar_t *ws=wide(stage);check(ws&&same_file_bytes(image,ws),"staging bytes independently equal the actual loaded PE");free(ws);
 check(fo_c_driver_publish(root,stage,digest,pin,sizeof(pin))==0,"exclusive native digest publication succeeds");
 wchar_t *wp=wide(pin);check(wp&&same_file_bytes(image,wp),"published bytes independently remain exact");
 check(fo_c_driver_validate_pin(pin,&size)==0&&size>0,"published readonly PE validates with actual private ownership");
 if(wp){STARTUPINFOW si={.cb=sizeof(si)};PROCESS_INFORMATION pi;wchar_t cmd[32768];_snwprintf(cmd,32768,L"\"%ls\" --pinned-child",wp);int ok=CreateProcessW(wp,cmd,NULL,NULL,FALSE,0,NULL,NULL,&si,&pi);check(ok,"published pin runs as a native PE");if(ok){DWORD code=0;WaitForSingleObject(pi.hProcess,10000);GetExitCodeProcess(pi.hProcess,&code);check(code==17,"native published child returns exact independent exit17");CloseHandle(pi.hThread);CloseHandle(pi.hProcess);}}
 check(fo_c_driver_stage_copy(root,stage2,sizeof(stage2))==0,"second owner stages complete image");
 check(fo_c_driver_publish(root,stage2,digest,pin2,sizeof(pin2))==1,"digest collision never replaces published image");
 check(wp&&same_file_bytes(image,wp),"collision preserves original complete bytes");
 check(fo_c_driver_remove_stage(root,stage2)==0,"collision removes only its owned stage");
 snprintf(bad,sizeof(bad),"%s/not-a-pe",root);wchar_t *wb=wide(bad);if(wb){int f=open(bad,O_CREAT|O_EXCL|O_WRONLY,0600);check(f>=0,"creates actual current-user private invalid-image fixture");if(f>=0){write(f,"not PE",6);close(f);SetFileAttributesW(wb,FILE_ATTRIBUTE_READONLY);DWORD kind=0;check(!GetBinaryTypeW(wb,&kind),"independent Windows loader rejects readonly text");check(fo_c_driver_validate_pin(bad,&size)!=0,"readonly text cannot masquerade as native driver PE");SetFileAttributesW(wb,FILE_ATTRIBUTE_NORMAL);DeleteFileW(wb);}free(wb);}
 free(wp);printf("windows-driver: %d failures\n",errors);return errors?1:0;
}
