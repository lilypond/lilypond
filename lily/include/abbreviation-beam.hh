/*
  abbreviation-beam-engraver.hh -- declare Abbreviation_beam_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef ABBREVIATION_BEAM_HH
#define ABBREVIATION_BEAM_HH

#include "beam.hh"

/** a beam connects multiple stems Beam adjusts the stems its owns to
  make sure that they reach the beam and that point in the correct
  direction */
class Abbreviation_beam : public Beam {
public:
  

  Abbreviation_beam();

  VIRTUAL_COPY_CONS(Score_element);

protected:
  virtual void do_print() const;
  virtual Molecule stem_beams (Stem *here, Stem *next, Stem *prev) const;
  virtual Molecule* do_brew_molecule_p() const;
};

#endif // ABBREVIATION_BEAM_HH

