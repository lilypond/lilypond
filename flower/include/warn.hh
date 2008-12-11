/*
  warn.hh -- declare Error message functions

  source file of the LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef WARN_HH
#define WARN_HH

#include "std-string.hh"

void error (string s);
void message (string s);
void non_fatal_error (string);
void programming_error (string s);
void progress_indication (string s);
void warning (string s);

#endif /* WARN_HH */
