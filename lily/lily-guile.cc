/*
  lily-guile.cc -- implement assorted guile functions

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lily-guile.hh"

#ifdef __cplusplus
extern "C" {
#endif

SCM
gh_append (SCM a, SCM b)
{
  return gh_call2 (gh_eval_str ("append"), a, b);
}

SCM
gh_list1 (SCM a)
{
  return gh_call1 (gh_eval_str ("list"), a);
}

SCM
gh_list2(SCM a, SCM b)
{
  return gh_call2 (gh_eval_str ("list"), a, b);
}

SCM
gh_quote ()
{
  return gh_eval_str ("'quote");
}

SCM
gh_eval (SCM a)
{
  return gh_call1 (gh_eval_str ("eval"), a);
}

SCM
gh_lambda_o ()
{
  return gh_eval_str ("'(lambda (o))");
}

SCM
gh_func_o (char const* name)
{
  char buf[200];
  snprintf (buf, 200, "'(%s o)", name);
  return gh_eval_str (buf);
}

#ifdef __cplusplus
}
#endif
