/*
  abbreviation-beam.cc -- implement Abbreviation_beam

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "p-col.hh"
#include "array.hh"
#include "proto.hh"
#include "abbreviation-beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "atom.hh"
#include "molecule.hh"
#include "leastsquares.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem-info.hh"


IMPLEMENT_IS_TYPE_B1 (Abbreviation_beam, Spanner);

Abbreviation_beam::Abbreviation_beam ()
  : Beam ()
{
}

Molecule*
Abbreviation_beam::brew_molecule_p () const 
{
  /* 
   todo
   */
  return Beam::brew_molecule_p ();
#if 0
  Molecule *mol_p = new Molecule;
  // huh? inter-what
  //    Real inter_f = paper ()->interbeam_f ();
  Real inter_f = paper ()->internote_f ();
  Real x0 = stems[0]->hpos_f ();
  for (int j=0; j <stems.size (); j++) 
    {
      Stem *i = stems[j];
      Stem * prev = (j > 0)? stems[j-1] : 0;
      Stem * next = (j < stems.size ()-1) ? stems[j+1] :0;

      Molecule sb = stem_beams (i, next, prev);
      Real  x = i->hpos_f ()-x0;
      sb.translate (Offset (x, (x * slope_f  + left_y)* inter_f));
      mol_p->add (sb);
    }
  mol_p->translate_axis (x0 - spanned_drul_[LEFT]->absolute_coordinate (X_AXIS), X_AXIS);
  return mol_p;
#endif
}

void
Abbreviation_beam::do_print () const
{
#ifndef NPRINT
  Beam::do_print ();
  Spanner::do_print ();
#endif
}

/*
  beams to go with one stem.
  */
Molecule
Abbreviation_beam::stem_beams (Stem *here, Stem *next, Stem *prev) const
{
  /* 
   todo
    - shorter beams (not reaching outer "stems") 
      for [:16 c4 c4] and [:16 c1 c1]
    - centered beam on [:16 c1 c1] heads, rather than "stems"
   */
  return Beam::stem_beams (here, next, prev);
}
