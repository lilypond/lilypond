/*
  atom.hh -- declare Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef ATOM_HH
#define ATOM_HH

#include "protected-scm.hh"
#include "string.hh"
#include "box.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"
#include "smobs.hh"

/**
   Atoms should only be created on the heap, ie. with
   "new Atom"
 */
class Atom {
  Offset off_;

  friend class Molecule;
  friend class Paper_outputter;
public:
  Atom (SCM s);
  Atom (Atom const&);
  
  DECLARE_SMOBS;

  /*
    SCM expression that (when evaluated) gives a TeX string
representing a musical notation symbol.  */
  SCM func_;
  void fontify (Font_metric*);
};

Atom* unsmob_atom (SCM);

#endif
