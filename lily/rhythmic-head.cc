/*
  rhythmic-head.cc -- implement Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "rhythmic-head.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "rest.hh"
#include "dots.hh"
#include "axis-group-element.hh"
#include "paper-score.hh"
#include "stem.hh"



int
Rhythmic_head::dots_i () const
{
  return dots_l_ ? dots_l_->dots_i_ : 0;
}
  
void
Rhythmic_head::do_post_processing ()
{
  if (dots_l_)
    {
      dots_l_->position_i_ = int (position_f ());
    }
}

void
Rhythmic_head::do_pre_processing ()
{
  translate_axis (position_i_ * staff_line_leading_f () /2.0, Y_AXIS);
  position_i_ = 0;
}

Real
Rhythmic_head::position_f () const
{
  return position_i_ +  Staff_symbol_referencer::position_f ();
}


void
Rhythmic_head::add_dots (Dots *dot_l)
{
  dots_l_ = dot_l;  
  dot_l->add_dependency (this);  
}

Rhythmic_head::Rhythmic_head ()
{
  dots_l_ =0;
  balltype_i_ =0;
  stem_l_ =0;
  position_i_ =0;
}

void
Rhythmic_head::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  Staff_symbol_referencer::do_substitute_element_pointer (o,n);
  if (o == dots_l_)
    dots_l_ = dynamic_cast<Dots *> (n) ;
  else if (o == stem_l_)
    stem_l_ = dynamic_cast<Stem*>(n);
}


void
Rhythmic_head::do_print () const
{
#ifndef NPRINT
  DOUT << "balltype = "<< balltype_i_ << "dots = " << dots_i ();
#endif
}

