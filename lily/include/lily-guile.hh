/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include "config.hh"
#include  "string.hh"

#include <guile/gh.h>
#include <libguile.h>

SCM ly_symbol (String name);
String symbol_to_string (SCM);
SCM ly_set_scm (String name , SCM val);

SCM ly_append (SCM a, SCM b);
SCM ly_eval (SCM a);
SCM ly_func_o (char const* name);
SCM ly_quote_scm (SCM s);
void ly_display_scm (SCM s);
String ly_scm2string (SCM s);

#include "array.hh"
#include "scalar.hh"


void read_lily_scm_file (String);
void init_symbols ();
#include "ly-symbols.hh"

#endif // LILY_GUILE_HH
