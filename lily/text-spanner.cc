/*
  text-spanner.cc -- implement Text_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "box.hh"
#include "text-spanner.hh"
#include "text-def.hh"
#include "debug.hh"
#include "paper-def.hh"



void
Text_spanner::set_support (Directional_spanner*d)
{
  if (support_span_l_)
    remove_dependency (support_span_l_);
  
  support_span_l_ =d;
  add_dependency (d);
}

void
Text_spanner::do_add_processing ()
{
  set_bounds (LEFT, support_span_l_->spanned_drul_[LEFT]);
  set_bounds (RIGHT, support_span_l_->spanned_drul_[RIGHT]);
}

Text_spanner::Text_spanner()
{
  spec_p_ = 0;
  support_span_l_ = 0;
}




void
Text_spanner::do_print() const
{
  spec_p_->print();
}

void
Text_spanner::do_post_processing()
{
  text_off_ = support_span_l_->center() +
	Offset (0,support_span_l_->dir_ * paper()->internote_f () * 4); // todo
}

Molecule*
Text_spanner::do_brew_molecule_p() const
{
  Molecule tsym (spec_p_->get_molecule (paper(),CENTER));
  tsym.translate (text_off_);

  Molecule*output = new Molecule;
  output->add_molecule (tsym);
  return output;
}

void
Text_spanner::do_pre_processing()
{
  spanned_drul_ = support_span_l_->spanned_drul_;
}

Interval
Text_spanner::height() const
{
  return do_brew_molecule_p()->extent ().y ();
}

void
Text_spanner::do_substitute_dependency (Score_element* o, Score_element*n)
{
  if (support_span_l_ == o) 
	support_span_l_ = (Directional_spanner*) (n?dynamic_cast <Spanner *> (n):0);
}


Text_spanner::~Text_spanner()
{
  delete spec_p_;
}

Text_spanner::Text_spanner (Text_spanner const&s)
  : Spanner (s)
{
  support_span_l_ = s.support_span_l_;
  spec_p_ = s.spec_p_? s.spec_p_->clone() : 0;
  text_off_ = s.text_off_;
}
