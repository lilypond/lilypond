/*
  libc-extension.hh -- declare some string.h extensions

  source file of the flowerlib

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef LIBC_EXTENSION_HH
#define LIBC_EXTENSION_HH

#include <stddef.h>
#include <stdarg.h>

#include "config.hh"

char *strnlwr (char *start, int n);
char *strnupr (char *start, int n);

#if ! HAVE_MEMMEM		/* GNU extension. */
void *memmem (void const* haystack, int haystack_len,
	      void const *needle, int needle_len);
#endif /* HAVE_MEMMEM */

#if ! HAVE_SNPRINTF		/* GNU extension. */
int snprintf (char *str, size_t n, char const *format, ...);
#endif

#if ! HAVE_VSNPRINTF	 	/* GNU extension. */
int vsnprintf (char *str, size_t, char const *format, va_list args);
#endif

#ifndef isinf
#if ! HAVE_ISINF		/* BSD extension. */
int isinf (double x);
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if ! HAVE_FOPENCOOKIE          /* GNU extension. */

#include <stdio.h>
#include <unistd.h>

#if (! defined (__off64_t) && ! defined (__off64_t_defined)) || ! defined (__cplusplus)
#define off64_t unsigned long long
#endif

#if HAVE_LIBIO_H
#include <libio.h>
#else

#if 1 //! HAVE_FUNOPEN

#define cookie_io_functions_t le_cookie_io_functions_t 
  typedef struct
  {
    ssize_t (*read) (void *, char *, size_t);
    ssize_t (*write) (void *, char const *, size_t);
    int (*seek) (void *, off64_t *, int);
    int (*close) (void *);
  } cookie_io_functions_t;

#else

  typedef struct
  {
    int (*read) (void *, char *, int);
    int (*write) (void *, char const *, int);
    fpos_t (*seek) (void *, fpos_t, int);
    int (*close) (void *);
  } cookie_io_functions_t;

#endif /* ! HAVE_FUNOPEN */
#endif /* ! HAVE_LIBIO_H */

  FILE *fopencookie (void *cookie, char const *modes,
		     cookie_io_functions_t io_funcs);

#if ! HAVE_FUNOPEN

  int handle_cookie_io_fclose (FILE *);
  int handle_cookie_io_fprintf (FILE *file, char const *format, ...);
  int handle_cookie_io_putc (int c, FILE *file);

/* FIXME: ttftool uses fclose fopencookie fprintf and putc only.  if
          ALIAS_FILE_TO_FILECOOKIE, blondly redefine those functions
          to wrappers that check for and handle Memory_out_stream.  */
#ifdef ALIAS_FILE_TO_FILECOOKIE

#define fclose handle_cookie_io_fclose
#define fprintf handle_cookie_io_fprintf
#ifdef putc
#define std_putc putc
#undef putc
#endif
#define putc handle_cookie_io_putc

#endif /* ALIAS_FILE_TO_FILECOOKIE */
#endif /* ! HAVE_FUNOPEN */
#endif /* ! HAVE_FOPENCOOKIE */

#ifdef __cplusplus
} /* extern "C" */
#endif

unsigned char *memrchr (unsigned char const *p, int n, char c);
unsigned char *strrev (unsigned char *byte, int length_i);

double my_round (double);

#endif /* LIBC_EXTENSION_HH */
