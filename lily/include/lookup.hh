/*
  lookup.hh -- declare Lookup

  source file of the GNU LilyPond music typesetter

  (c) 1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LOOKUP_HH
#define LOOKUP_HH

#include "string.hh"
#include "molecule.hh"
#include "flower-proto.hh"
#include "direction.hh"
#include "box.hh"

struct Lookup
{
  static Molecule accordion (SCM arg, Real interline_f, Font_metric*fm);
  static Molecule frame (Box b, Real thick);
  static Molecule slur (Bezier controls, Real cthick, Real thick) ;
  static Molecule beam (Real, Real, Real) ;
  static Molecule dashed_slur (Bezier, Real thick, Real dash) ;
  static Molecule blank (Box b) ;
  static Molecule filledbox (Box b) ;
  static Molecule repeat_slash( Real w, Real slope, Real th);
};

#endif // LOOKUP_HH
