/*
  warn.cc -- implement warnings

  source file of the Flower Library

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "warn.hh"

#include <cstdlib>
#include <cstdio>

void
message (String s)
{
  fputs (s.to_str0 (), stderr);
  fflush (stderr);
}

void
warning (String s)
{
  message (_f ("warning: %s", s.to_str0 ()) + "\n");
}

void
non_fatal_error (String s)
{
  message (_f ("error: %s", s.to_str0 ()) + "\n");
}

void
error (String s)
{
  non_fatal_error (s);
  exit (1);
}

void
programming_error (String s)
{
  message (_f ("programming error: %s", s) + "\n");
  message (_ ("Continuing; crossing fingers") + "\n");
}

