/*
  abbreviation-beam.cc -- implement Abbreviation_beam

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "p-col.hh"
#include "array.hh"
#include "proto.hh"
#include "abbreviation-beam.hh"
#include "misc.hh"
#include "debug.hh"

#include "molecule.hh"
#include "leastsquares.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem-info.hh"




Abbreviation_beam::Abbreviation_beam ()
  : Beam ()
{
}

Molecule*
Abbreviation_beam::do_brew_molecule_p () const 
{
  return Beam::do_brew_molecule_p ();
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
