/*
  rest.cc -- implement Rest

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
Rest::do_brew_molecule_p () const
{
  bool streepjes_b = abs(position_i_) > lines_i () / 2 &&  
    (balltype_i_ == 0 || balltype_i_ == 1);
  
  String style; 
  SCM style_sym =get_elt_property (style_scm_sym);
  if (style_sym != SCM_BOOL_F)
    {
      style = ly_scm2string (SCM_CDR(style_sym));
    }
  
  Molecule s(lookup_l ()->rest (balltype_i_, streepjes_b, style));
  Molecule * m = new Molecule ( Molecule (s));
  m->translate_axis (position_i_ *  staff_line_leading_f ()/2.0, Y_AXIS);
  return m;
}


