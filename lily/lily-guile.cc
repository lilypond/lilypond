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
#include "debug.hh"


/*
  scm_m_quote doesn't use any env, but needs one for a good signature in GUILE.

  Why there is no gh_quote () in GUILE  beats me.
*/

SCM
ly_quote_scm (SCM s)
{
  return scm_cons2 (scm_i_quote, s, SCM_EOL);
}

/*
  See: libguile/symbols.c

  SCM
  scm_string_to_symbol(s)
  
*/
SCM
ly_symbol (String name)
{
  return gh_car (scm_intern (name.ch_C(), name.length_i()));
}

String
symbol_to_string (SCM s)
{
  return String((Byte*)SCM_CHARS (s), (int) SCM_LENGTH(s));
}

SCM
ly_set_scm (String name, SCM val)
{
  return scm_sysintern (name.ch_C(), val);
  
}
/**

   Read a file, and shove it down GUILE.  GUILE also has file read
   functions, but you can't fiddle with the path of those.
   
 */
void
read_lily_scm_file (String fn)
{
  String s = global_path.find (fn);
  if (s == "")
    {
      String e = _f ("Can not find file `%s\'", fn);
      e += " ";
      e += _f ("(Load path is `%s\'", global_path.str ());
      error (e);
    }
  else
    *mlog << '[' << s;


  Simple_file_storage f(s);
  
  gh_eval_str ((char *) f.ch_C());
  *mlog << ']' << flush;  
}


void
ly_display_scm (SCM s)
{
  gh_display (s);
  gh_newline ();
}
