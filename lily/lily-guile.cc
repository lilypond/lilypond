/*
  lily-guile.cc -- implement assorted guile functions

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <stdio.h>
#include "libc-extension.hh"
#include "lily-guile.hh"
#include "main.hh"

#ifdef __cplusplus
extern "C" {
#endif

SCM
ly_append (SCM a, SCM b)
{
  return gh_call2 (gh_eval_str ("append"), a, b);
}

SCM
ly_list1 (SCM a)
{
  return gh_call1 (gh_eval_str ("list"), a);
}

SCM
ly_list2(SCM a, SCM b)
{
  return gh_call2 (gh_eval_str ("list"), a, b);
}

SCM
ly_quote ()
{
  return gh_eval_str ("'quote");
}

SCM
ly_eval (SCM a)
{
  return gh_call1 (gh_eval_str ("eval"), a);
}

SCM
ly_lambda_o ()
{
  return gh_eval_str ("'(lambda (o))");
}

SCM
ly_func_o (char const* name)
{
  char buf[200];
  snprintf (buf, 200, "'(%s o)", name);
  return gh_eval_str (buf);
}

#ifdef __cplusplus
}
#endif

SCM
lambda_scm (String str, Array<int> args_arr)
{
  if (str.empty_b ())
    {
      str = "empty";
      args_arr.clear ();
    }
  SCM args_scm = SCM_EOL;
  for (int i = args_arr.size () - 1; i >= 0; i--)
    args_scm = gh_cons (gh_int2scm (args_arr[i]), args_scm);
  SCM scm =
    ly_append (ly_lambda_o (), 
    ly_list1 (ly_append (ly_func_o (str.ch_l ()), args_scm)));
  return scm;
}

SCM
lambda_scm (String str, Array<Scalar> args_arr)
{
  if (str.empty_b ())
    {
      str = "empty";
      args_arr.clear ();
    }
  SCM args_scm = SCM_EOL;
  for (int i = args_arr.size () - 1; i >= 0; i--)
    args_scm = gh_cons (gh_str02scm (args_arr[i].ch_l ()), args_scm);
  SCM scm =
    ly_append (ly_lambda_o (), 
    ly_list1 (ly_append (ly_func_o (str.ch_l ()), args_scm)));
  return scm;
}

SCM
lambda_scm (String str, Array<Real> args_arr)
{
  if (str.empty_b ())
    {
      str = "empty";
      args_arr.clear ();
    }
  SCM args_scm = SCM_EOL;
  for (int i = args_arr.size () - 1; i >= 0; i--)
    args_scm = gh_cons (gh_double2scm (args_arr[i]), args_scm);
  SCM scm =
    ly_append (ly_lambda_o (), 
    ly_list1 (ly_append (ly_func_o (str.ch_l ()), args_scm)));
  return scm;
}


