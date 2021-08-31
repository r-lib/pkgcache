
#include "errors.h"

#ifdef WIN32

#include <windows.h>

static int utf8_to_utf16(const char* s, WCHAR** ws_ptr) {
  int ws_len, r;
  WCHAR* ws;

  ws_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

  if (ws_len <= 0) { return GetLastError(); }

  /* Jumps on error */
  ws = (WCHAR*) R_alloc(ws_len,  sizeof(WCHAR));

  r = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ ws,
    /* cchWideChar =    */ ws_len);

  if (r != ws_len) {
    error("processx error interpreting UTF8 command or arguments");
  }

  *ws_ptr = ws;
  return 0;
}

int open_file(const char *path, int oflag) {
  WHAT *wpath;
  int ret = utf8_to_utf16(path, &wpath);
  if (ret) R_THROW_SYSTEM_ERROR_CODE(ret, "Failed to open `%s`", path);
  return _wopen(wpath, oflag);
}

#else

#include <fcntl.h>

 int open_file(const char *path, int oflag) {
  return open(path, oflag);
}

#endif
