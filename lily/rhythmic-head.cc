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


Dots*
Rhythmic_head::dots_l () const
{
  SCM s = get_elt_property ("dot");
  return dynamic_cast<Dots*> (unsmob_element (s));
}

Stem*
Rhythmic_head::stem_l () const
{
  SCM s = get_elt_property ("stem");
  return dynamic_cast<Stem*> (unsmob_element (s));
}

int
Rhythmic_head::dots_i () const
{
  return dots_l () ? dots_l ()->dots_i_ : 0;
}
  
void
Rhythmic_head::do_post_processing ()
{
  if (dots_l ())
    {
      dots_l ()->set_position(int (position_f ()));
    }
}


void
Rhythmic_head::add_dots (Dots *dot_l)
{
  set_elt_property ("dot", dot_l->self_scm_);
  dot_l->add_dependency (this);  
}

Rhythmic_head::Rhythmic_head ()
{
  balltype_i_ =0;
}



void
Rhythmic_head::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << "balltype = "<< balltype_i_ << "dots = " << dots_i ();
#endif
}

