/*
  atom.hh -- declare Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef ATOM_HH
#define ATOM_HH

#include "protected-scm.hh"
#include "string.hh"
#include "box.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"

class Atom {
public:
  Atom (SCM s);
  Offset off_;
  /*
    SCM expression that (when evaluated) gives a TeX string
representing a musical notation symbol.  */
  Protected_scm func_;
  Protected_scm font_;
};


#endif
