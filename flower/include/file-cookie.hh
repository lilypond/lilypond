#ifdef __cplusplus
extern "C" {
#endif

#include <unistd.h>

#include <cstdio>
  using namespace std;

#if (! defined (__off64_t) && ! defined (__off64_t_defined)) || ! defined (__cplusplus)
#define off64_t unsigned long long
#endif

  typedef struct
  {
    ssize_t (*read) (void *, char *, size_t);
    ssize_t (*write) (void *, char const *, size_t);
    ssize_t (*seek) (void *, off64_t *, size_t);
    ssize_t (*close) (void *);
  } lily_cookie_io_functions_t;

  void *lily_fopencookie (void *cookie, char const *modes,
                          lily_cookie_io_functions_t io_funcs);

  ssize_t lily_cookie_fclose (void *);
  ssize_t lily_cookie_fprintf (void *file, char const *format, ...)
  __attribute__ ((format (printf, 2, 3)));
  ssize_t lily_cookie_putc (int c, void *file);

#ifdef __cplusplus
} /* extern "C" */
#endif
