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
    int (*seek) (void *, off64_t *, int);
    int (*close) (void *);
  } lily_cookie_io_functions_t;

  void *lily_fopencookie (void *cookie, char const *modes,
			  lily_cookie_io_functions_t io_funcs);

  int lily_cookie_fclose (void *);
  int lily_cookie_fprintf (void *file, char const *format, ...)
    __attribute__ ((format (printf, 2, 3)));
  int lily_cookie_putc (int c, void *file);

#ifdef __cplusplus
} /* extern "C" */
#endif
