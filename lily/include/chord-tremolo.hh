/*
  chord-tremolo-engraver.hh -- declare Chord_tremolo_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef Chord_tremolo_HH
#define Chord_tremolo_HH

#include "beam.hh"

/** a beam connects multiple stems Beam adjusts the stems its owns to
  make sure that they reach the beam and that point in the correct
  direction */
class Chord_tremolo : public Beam {
public:
  

  Chord_tremolo();

  VIRTUAL_COPY_CONS(Score_element);

protected:
  virtual void do_print() const;
  virtual Molecule stem_beams (Stem *here, Stem *next, Stem *prev) const;
  virtual Molecule* do_brew_molecule_p() const;
};

#endif // Chord_tremolo_HH

