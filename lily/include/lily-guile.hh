/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include "config.hh"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_LIBGUILE
extern "C" { 
#include <guile/gh.h> 
}
#else
typedef long SCM;
#endif

SCM gh_append (SCM a, SCM b);
SCM gh_eval (SCM a);
SCM gh_func_o (char const* name);
SCM gh_lambda_o ();
SCM gh_list1 (SCM a);
SCM gh_list2(SCM a, SCM b);
SCM gh_quote ();

#ifdef __cplusplus
}
#endif

#endif // LILY_GUILE_HH
