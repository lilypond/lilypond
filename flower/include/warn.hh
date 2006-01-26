/*
  warn.hh -- declare Error message functions

  source file of the LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef WARN_HH
#define WARN_HH

#include "std-string.hh"

void error (std::string s);
void message (std::string s);
void non_fatal_error (std::string);
void programming_error (std::string s);
void progress_indication (std::string s);
void warning (std::string s);

#endif /* WARN_HH */
