/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include  "string.hh"

#include <guile/gh.h>
#include <libguile.h>
#include "direction.hh"

SCM ly_ch_C_to_scm (char const*c);
SCM ly_ch_C_eval_scm (char const*c);
SCM ly_symbol (String name);
String symbol_to_string (SCM);
SCM ly_set_scm (String name , SCM val);

SCM ly_append (SCM a, SCM b);
SCM ly_eval (SCM a);
SCM ly_func_o (char const* name);
SCM ly_quote_scm (SCM s);
void ly_display_scm (SCM s);
String ly_scm2string (SCM s);
SCM array_to_list (SCM *a , int l);


#include "array.hh"
#include "scalar.hh"


void read_lily_scm_file (String);
void init_lily_guile ();

#include "ly-symbols.hh"

/*
  Do It Yourself GC protection.
 */
SCM ly_protect_scm (SCM s);
SCM ly_unprotect_scm (SCM s);
void init_ly_protection ();

SCM index_cell (SCM cellp, Direction d);

#endif // LILY_GUILE_HH
