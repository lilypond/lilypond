/*
  lily-guile.cc -- implement assorted guile functions

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>
#include "libc-extension.hh"
#include "lily-guile.hh"
#include "main.hh"
#include "simple-file-storage.hh"
#include "file-path.hh"

SCM
ly_list1 (SCM a)
{
  return gh_list (a, SCM_UNDEFINED);
}

SCM
ly_quote ()
{
  return gh_eval_str ("'quote");
}

/*
  scm_m_quote doesn't use any env, but needs one for a good signature in GUILE.

  Why there is no gh_quote () in GUILE  beats me.
*/

SCM
ly_quote_scm (SCM s)
{
  //  return scm_m_quote (s, SCM_UNDEFINED);
  return scm_cons2 (scm_i_quote, s, SCM_EOL);
  
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
  char buf[200];		// ugh.
  snprintf (buf, 200, "'(%s o)", name);
  return gh_eval_str (buf);
}


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
    gh_append2 (ly_lambda_o (), 
		ly_list1 (gh_append2 (ly_func_o (str.ch_l ()), args_scm)));
  return scm;
}

// scm_top_level_env(SCM_CDR(scm_top_level_lookup_closure_var)))
SCM
lambda_scm (String str, Array<Scalar> args_arr)
{
  if (str.empty_b ())
    {
      str = "empty";
      args_arr.clear ();
    }
  SCM args_scm = SCM_EOL;
  for (int i = args_arr.size (); i--; )
    args_scm = gh_cons (gh_str02scm (args_arr[i].ch_l ()), args_scm);
  SCM scm =
    gh_append2 (ly_lambda_o (), 
    ly_list1 (gh_append2 (ly_func_o (str.ch_l ()), args_scm)));
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
  for (int i = args_arr.size (); i--; )
    args_scm = gh_cons (gh_double2scm (args_arr[i]), args_scm);
  
  SCM scm =
    gh_append2 (ly_lambda_o (), 
    ly_list1 (gh_append2 (ly_func_o (str.ch_l ()), args_scm)));
  return scm;
}

/**

   Read a file, and shove it down GUILE.  GUILE also has file read
   functions, but you can't fiddle with the path of those.
   
 */

void
read_lily_scm_file (String fn)
{
  String s = global_path.find (fn);
  Simple_file_storage f(s);
  
  gh_eval_str ((char *) f.ch_C());
}
