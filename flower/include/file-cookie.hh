#ifdef __cplusplus
extern "C" {
#endif


#include <stdio.h>
#include <unistd.h>

#if (! defined (__off64_t) && ! defined (__off64_t_defined)) || ! defined (__cplusplus)
#define off64_t unsigned long long
#endif
 
  typedef struct
  {
    int (*read) (void *, char *, size_t);
    int (*write) (void *, char const *, size_t);
    int (*seek) (void *, off64_t *, int);
    int (*close) (void *);
  } lily_cookie_io_functions_t;


  void *lily_fopencookie (void *cookie, char const *modes,
			  lily_cookie_io_functions_t io_funcs);

  int lily_cookie_fclose (void *);
  int lily_cookie_fprintf (void *file, char const *format, ...);
  int lily_cookie_putc (int c, void *file);

#ifdef __cplusplus
} /* extern "C" */
#endif
