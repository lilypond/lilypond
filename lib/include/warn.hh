/*
  warn.hh -- declare Error message functions

  source file of the LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef WARN_HH
#define WARN_HH

#include "string.hh"

void warning (String message_str  );
void error (String message_str);
void non_fatal_error (String);
#endif // WARN_HH
