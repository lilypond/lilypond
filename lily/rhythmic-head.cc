/*
  rhythmic-head.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "rhythmic-head.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "rest.hh"
#include "dots.hh"
#include "axis-group-element.hh"
#include "p-score.hh"

void
Rhythmic_head::do_add_processing ()
{
  if (dots_i_ && !dots_l_)
    {
      Dots *d = new Dots;
      add (d);
      pscore_l_->typeset_element (d);
      axis_group_l_a_[Y_AXIS]->add_element (d);
      axis_group_l_a_[X_AXIS]->add_element (d);
    }
  if (dots_l_)
    {
      dots_l_->no_dots_i_ = dots_i_;
    }
}

void
Rhythmic_head::add (Dots *dot_l)
{
  dots_l_ = dot_l;  
  dot_l->add_dependency (this);  
}


Rhythmic_head::Rhythmic_head ()
{
  dots_l_ =0;
  balltype_i_ =0;
  dots_i_ = 0;
}

void
Rhythmic_head::do_substitute_dependent (Score_elem*o,Score_elem*n)
{
  if (o == dots_l_)
    dots_l_ = n ? (Dots*)n->item () :0;
}

IMPLEMENT_IS_TYPE_B1(Rhythmic_head, Item);
void
Rhythmic_head::do_print () const
{
#ifndef NPRINT
  DOUT << "balltype = "<< balltype_i_ << "dots = " << dots_i_;
#endif
}

