/*
  atom.cc -- implement Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "atom.hh"
#include "interval.hh"
#include "string.hh"
#include "array.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "lookup.hh"
#include "main.hh"
#include "global-ctor.hh"

Atom::Atom(SCM s)
{
  func_ = s;
}
