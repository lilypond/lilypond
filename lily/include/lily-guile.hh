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

SCM ly_append (SCM a, SCM b);
SCM ly_eval (SCM a);
SCM ly_func_o (char const* name);
SCM ly_lambda_o ();
SCM ly_list1 (SCM a);
SCM ly_list2(SCM a, SCM b);
SCM ly_quote ();

#ifdef __cplusplus
}
#endif

#include "array.hh"
#include "scalar.hh"
SCM lambda_scm (String str, Array<Real> args_arr);
SCM lambda_scm (String str, Array<Scalar> args_arr);

#endif // LILY_GUILE_HH
