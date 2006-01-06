/*
  warn.hh -- declare Error message functions

  source file of the LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef WARN_HH
#define WARN_HH

#include "string.hh"

void error (String s);
void message (String s);
void non_fatal_error (String);
void programming_error (String s);
void progress_indication (String s);
void warning (String s);

#endif /* WARN_HH */
