/*
  abbreviation-beam.cc -- implement Chord_tremolo

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-column.hh"
#include "array.hh"
#include "proto.hh"
#include "chord-tremolo.hh"
#include "misc.hh"
#include "debug.hh"

#include "molecule.hh"
#include "leastsquares.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem-info.hh"




Chord_tremolo::Chord_tremolo ()
  : Beam ()
{
}

Molecule*
Chord_tremolo::do_brew_molecule_p () const 
{
  return Beam::do_brew_molecule_p ();
}

void
Chord_tremolo::do_print () const
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
Chord_tremolo::stem_beams (Stem *here, Stem *next, Stem *prev) const
{
  /* 
   todo
    - shorter beams (not reaching outer "stems") 
      for [:16 c4 c4] and [:16 c1 c1]
    - centered beam on [:16 c1 c1] heads, rather than "stems"
   */
  return Beam::stem_beams (here, next, prev);
}
