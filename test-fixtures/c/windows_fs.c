#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "fo_driver_utf8.h"
int fo_c_mkdir_p(const char *);
int fo_c_rm_rf(const char *);
int fo_c_collect_files(const char *,const char *,const char *,const char *,int,char *,int,int);
int fo_c_stat_identity(const char *,long long *,long long *);
int fo_c_realpath(const char *,char *,int);
static int errors;
static void check(int yes,const char *label){printf("%s: %s errno=%d winerror=%lu\n",yes?"PASS":"FAIL",label,errno,GetLastError());if(!yes)++errors;}
static char *join(const char *a,const char *b){size_t n=strlen(a)+strlen(b)+2;char *p=malloc(n);if(p)snprintf(p,n,"%s/%s",a,b);return p;}
static wchar_t *wide(const char *s){int n=MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,s,-1,NULL,0);wchar_t *w=calloc((size_t)n,sizeof(*w));if(!w||!MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,s,-1,w,n)){free(w);return NULL;}return w;}
static int put(const char *p){wchar_t *w=wide(p);if(!w)return 0;HANDLE h=CreateFileW(w,GENERIC_WRITE,FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,NULL,CREATE_NEW,0,NULL);free(w);if(h==INVALID_HANDLE_VALUE)return 0;DWORD n;int ok=WriteFile(h,"known\0bytes",11,&n,NULL)&&n==11;CloseHandle(h);return ok;}
int main(int argc,char **argv){
 if(argc!=2)return 2;
 char *root=argv[1],*bin=join(root,"bin"),*file=join(bin,"native-\xce\xbb.dat"),*alias=join(root,"alias"),*missing=join(root,"missing"),out[8192],real[4096];
 check(bin&&file&&alias&&missing,"allocates bounded fixture path closure");if(!bin||!file||!alias||!missing)return 2;
 check(fo_c_mkdir_p(bin)==0,"native UTF8 recursive directory creation");check(put(file),"independent Win32 binary fixture creation");
 check(fo_c_collect_files(root,"native-",".dat","/bin/",1,out,sizeof(out),0)==1&&!strcmp(out,file),"collector includes final directory separator and exact UTF8 file");
 check(fo_c_collect_files(root,"",".dat","",1,out,4,0)==-1,"collector reports capacity exhaustion rather than truncation");
 check(fo_c_collect_files(missing,"","","",1,out,sizeof(out),1)==0,"missing collector root is empty");
 check(fo_c_collect_files(file,"","","",1,out,sizeof(out),1)==-1,"ordinary file is a hard collector root error");
 long long device=0,inode=0;wchar_t *wf=wide(file);HANDLE f=CreateFileW(wf,FILE_READ_ATTRIBUTES,FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,NULL,OPEN_EXISTING,0,NULL);BY_HANDLE_FILE_INFORMATION info;
 check(f!=INVALID_HANDLE_VALUE&&GetFileInformationByHandle(f,&info)&&fo_c_stat_identity(file,&device,&inode)==0&&(uint64_t)device==info.dwVolumeSerialNumber&&(uint64_t)inode==(((uint64_t)info.nFileIndexHigh<<32)|info.nFileIndexLow),"file identity equals independent held Win32 file identity");if(f!=INVALID_HANDLE_VALUE)CloseHandle(f);
 check(fo_c_realpath(file,real,sizeof(real))==0&&strstr(real,"native-\xce\xbb.dat"),"physical native path preserves Unicode bytes");
 if(strncmp(root,"//",2)&&strncmp(root,"\\\\",2)){
  wchar_t *wa=wide(alias),*wb=wide(bin);BOOL linked=CreateSymbolicLinkW(wa,wb,SYMBOLIC_LINK_FLAG_DIRECTORY|SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE);
  check(linked,"independent Windows directory reparse alias creates");
  if(linked){check(fo_c_collect_files(root,"",".dat","",1,out,sizeof(out),1)==-2,"strict collection rejects actual reparse namespace");check(fo_c_collect_files(root,"",".dat","",1,out,sizeof(out),0)==1,"ordinary collection ignores directory alias without duplicate traversal");check(fo_c_rm_rf(alias)==0&&GetFileAttributesW(wf)!=INVALID_FILE_ATTRIBUTES,"alias cleanup never traverses or removes its target");}
  free(wa);free(wb);
 }
 int rc=fo_c_rm_rf(root);printf("NATIVE_RECURSIVE_CLEANUP rc=%d errno=%d winerror=%lu\n",rc,errno,GetLastError());check(rc==0,"owned native recursive cleanup succeeds");
 if(rc){BOOL removed=DeleteFileW(wf);printf("INDEPENDENT_DELETE_FILE rc=%d winerror=%lu\n",removed,GetLastError());}
 free(wf);free(bin);free(file);free(alias);free(missing);printf("windows-fs: %d failures\n",errors);return errors?1:0;
}
