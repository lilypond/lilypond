/*
  warn.cc -- implement warning and error messages. Needs cleanup.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "my-lily-lexer.hh"
#include "moment.hh"
#include "source-file.hh"
#include "source.hh"
#include "main.hh"
#include "input.hh"

ostream &warnout (cerr);

void
progress_indication (String s)
{
  cerr << s << flush;
}

