/*
  chord-name.hh -- declare Chord_name

  source file of the GNU LilyPond music typesetter

  (c) 1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_NAME_HH
#define CHORD_NAME_HH

#include "lily-guile.hh"
#include "molecule.hh"

/**
   elt_properties:
   pitches: list of musical-pitch
   inversion(optional): musical-pitch
   bass(optional): musical-pitch
 */
class Chord_name
{
public:
  static SCM brew_molecule (SCM);
  static Molecule ly_word2molecule (Score_element*, SCM scm, Real* x) ;
  static Molecule ly_text2molecule (Score_element*, SCM scm) ;
  static SCM after_line_breaking (SCM);
};

#endif // CHORD_NAME_HH
