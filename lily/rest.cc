/*
  rest.cc -- implement Rest

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "rest.hh"
#include "dots.hh"
#include "axis-group-element.hh"
#include "p-score.hh"

void
Rest::do_add_processing ()
{
  if (balltype_i_ > 1)
    position_i_ -= 4;
  else if (balltype_i_ == 0)
    position_i_ += 2;

  Rhythmic_head::do_add_processing ();
  if (dots_l_ && balltype_i_ > 1)
    {
      dots_l_->position_i_ = position_i_ + 4;
    }
}

Rest::Rest ()
{
  position_i_ =0;
}

Molecule *
Rest::brew_molecule_p () const
{
  int staff_size_i_ = 8;
  bool streepjes_b = abs(position_i_) > staff_size_i_ /2 &&  
    (balltype_i_ == 0 || balltype_i_ == 1);
  
  Atom s(lookup_l ()->rest (balltype_i_, streepjes_b));
  Molecule * m = new Molecule ( Atom (s));
  m->translate_axis (position_i_ *  paper ()->internote_f (), Y_AXIS);
  return m;
}


