/*
  lily-guile.cc -- implement assorted guile functions

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>

  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <stdio.h>
#include <stdlib.h>

#include "libc-extension.hh"
#include "lily-guile.hh"
#include "main.hh"
#include "simple-file-storage.hh"
#include "file-path.hh"
#include "debug.hh"

SCM
ly_ch_C_to_scm (char const*c)
{
  // this all really sucks, guile should take char const* arguments!
  return gh_str02scm ((char*)c);
}

SCM
ly_ch_C_eval_scm (char const*c)
{
  // this all really sucks, guile should take char const* arguments!
  return gh_eval_str ((char*)c);
}

  
/*
  Pass string to scm parser, evaluate one expression.
  Return result value and #chars read.

  Thanks to Gary Houston <ghouston@freewire.co.uk>

  Need guile-1.3.4 (>1.3 anyway) for ftell on str ports -- jcn
*/
SCM
ly_parse_scm (char const* s, int* n)
{
  SCM str = gh_str02scm ((char*)s);
  SCM port = scm_mkstrport (SCM_INUM0, str, SCM_OPN | SCM_RDNG,
                            "scm_eval_0str");
  SCM from = scm_ftell (port);

  SCM form;
  SCM answer = SCM_UNSPECIFIED;

  /* Read expression from port */
  if (!SCM_EOF_OBJECT_P (form = scm_read (port)))
    answer = scm_eval_x (form);

  /*
   After parsing

       (begin (foo 1 2))

   all seems fine, but after parsing

       (foo 1 2)

   read_buf has been advanced to read_pos - 1,
   so that scm_ftell returns 1, instead of #parsed chars
   */
  
  /*
    urg: reset read_buf for scm_ftell
    shouldn't scm_read () do this for us?
  */
  scm_fill_input (port);
  SCM to = scm_ftell (port);
  *n = gh_scm2int (to) - gh_scm2int (from);

  /* Don't close the port here; if we re-enter this function via a
     continuation, then the next time we enter it, we'll get an error.
     It's a string port anyway, so there's no advantage to closing it
     early.

     scm_close_port (port);
  */

  return answer;
}

/*
  scm_m_quote doesn't use any env, but needs one for a good signature in GUILE.
*/

SCM
ly_quote_scm (SCM s)
{
  return scm_m_quote (scm_cons2 (SCM_EOL, s, SCM_EOL) ,SCM_EOL); // apparently env arg is ignored.
}

/*
  See: libguile/symbols.c

  SCM
  scm_string_to_symbol(s)
  
*/
SCM
ly_symbol (String name)
{
  return gh_symbol2scm ((char*)name.ch_C());
}

String
symbol_to_string (SCM s)
{
  return String((Byte*)SCM_CHARS (s), (int) SCM_LENGTH(s));
}

SCM
ly_set_scm (String name, SCM val)
{
  return scm_sysintern ((char*)name.ch_C(), val);
  
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
      String e = _f ("Can't find file: `%s'", fn);
      e += " ";
      e += _f ("(load path: `%s')", global_path.str ());
      error (e);
    }
  else
    *mlog << '[' << s;


  Simple_file_storage f(s);
  
  ly_ch_C_eval_scm ((char *) f.ch_C());
  *mlog << "]" << flush;  
}


SCM
ly_gulp_file (SCM name)
{
  String fn (ly_scm2string (name));
 String s = global_path.find (fn);
  if (s == "")
    {
      String e = _f ("Can't find file: `%s'", fn);
      e += " ";
      e += _f ("(load path: `%s')", global_path.str ());
      error (e);
    }
  else
    *mlog << '[' << s;


  Simple_file_storage f(s);
  return ly_ch_C_to_scm (f.ch_C());
}

void
ly_display_scm (SCM s)
{
  gh_display (s);
  gh_newline ();
}

String
ly_scm2string (SCM s)
{
  int len; 
  char * p = gh_scm2newstr (s , &len);
  
  String r (p);

  free (p);
  return r;
}

SCM
index_cell (SCM s, Direction d)
{
  assert (d);
  return (d == LEFT) ? SCM_CAR (s) : SCM_CDR (s);
}

  
SCM
array_to_list (SCM *a , int l)
{
  SCM list = SCM_EOL;
  for (int i= l; i--;  )
    {
      list =  gh_cons (a[i], list);
    }
  return list;
}

SCM
ly_warning (SCM str)
{
  assert (gh_string_p (str));
  warning ("lily-guile: " + ly_scm2string (str));
  return SCM_BOOL_T;
}

void
init_functions ()
{
  scm_make_gsubr ("ly-warn", 1, 0, 0, (SCM(*)(...))ly_warning);
  scm_make_gsubr ("ly-gulp-file", 1,0, 0, (SCM(*)(...))ly_gulp_file);
}

ADD_SCM_INIT_FUNC(funcs, init_functions);


typedef void (*Void_fptr)();
Array<Void_fptr> *scm_init_funcs_;

void add_scm_init_func (void (*f)())
{
  if (!scm_init_funcs_)
    scm_init_funcs_ = new Array<Void_fptr>;

  scm_init_funcs_->push (f);
}

void
init_lily_guile ()
{
  for (int i=scm_init_funcs_->size() ; i--;)
    (scm_init_funcs_->elem (i)) ();
}

unsigned int ly_scm_hash (SCM s)
{
  return scm_ihashv (s, ~1u);
}
