/*
  warn.cc -- implement warning and error messages. Needs cleanup.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>
#include "string.hh"

void
progress_indication (String s)
{
 fputs (s.ch_C(), stderr);
 fflush (stderr);
}

