/*
  rhythmic-head.cc -- implement 

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

void
Rhythmic_head::do_add_processing ()
{
  if (dots_i_ && !dots_l_)
    {
      assert (false);
    }
  if (dots_l_)
    {
      dots_l_->dots_i_ = dots_i_;
    }
}

void
Rhythmic_head::do_post_processing ()
{
  if (dots_l_)
    {
      dots_l_->position_i_ = position_i_;
    }
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
  dots_i_ = 0;
  stem_l_ =0;
  position_i_ =0;
}

void
Rhythmic_head::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  if (o == dots_l_)
    dots_l_ = dynamic_cast<Dots *> (n) ;
  else if (o == stem_l_)
    stem_l_ = dynamic_cast<Stem*>(n);
}


void
Rhythmic_head::do_print () const
{
#ifndef NPRINT
  DOUT << "balltype = "<< balltype_i_ << "dots = " << dots_i_;
#endif
}

