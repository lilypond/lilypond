/*
  warn.hh -- declare Error message functions

  source file of the LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef WARN_HH
#define WARN_HH

#include "string.hh"

void error (String message_string);
void message (String s);
void non_fatal_error (String);
void programming_error (String s);
void warning (String message_string);

#define progress_indication message

#endif /* WARN_HH */
