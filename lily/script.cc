/*
  script.cc -- implement Script

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "script-def.hh"
#include "musical-request.hh"
#include "paper-def.hh"
#include "script.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"

void
Script::do_print () const
{
#ifndef NPRINT
  specs_p_->print ();
#endif
}

void
Script::do_substitute_dependency (Score_element*o,Score_element*n)
{
  Staff_side::do_substitute_dependency (o,n);
  if (o == stem_l_)
    {
      stem_l_ = dynamic_cast<Stem *> (n);
    }
}

void
Script::set_stem (Stem*st_l)
{
  stem_l_ = st_l;
  add_support (st_l);
}


Script::Script ()
{
  axis_ = Y_AXIS;
  specs_p_ = 0;
  stem_l_ = 0;
  postbreak_only_b_ = true;
  dir_ =  CENTER;
}

void
Script::set_default_dir ()
{
  int s_i=specs_p_->rel_stem_dir ();
  if (s_i)
    {
      if (stem_l_)
	dir_ = Direction (stem_l_->dir_ * s_i);
      else
	{
	  specs_p_->warning (_ ("script needs stem direction"));
	  dir_ = DOWN;
	}
    }
  else
    {
      dir_ =specs_p_->staff_dir ();
    }
  assert (dir_);
}


Interval
Script::do_width () const
{
  return specs_p_->get_atom (paper (), dir_).extent ().x ();
}

void
Script::do_pre_processing ()
{
  Staff_side::do_pre_processing ();
  if (breakable_b_ && postbreak_only_b_ && (break_status_dir () != RIGHT))
    {
      transparent_b_ = true;
      set_empty (true);
    }
  
  if (axis_ == Y_AXIS && !dir_)
    set_default_dir ();
}

Interval
Script::symbol_height () const
{
  return specs_p_->get_atom (paper (), dir_).extent ().y ();
}

Interval
Script::symbol_width () const
{
  return specs_p_->width (paper ());
}

Molecule*
Script::brew_molecule_p () const
{
  Real dx =0.;

  Molecule*mol_p = new Molecule (specs_p_->get_atom (paper (), dir_));
  /*
    ugh, staccato dots are not centred between stafflines (how?)?
  */
  Real correct =0.0;
  if (axis_ == Y_AXIS){
    dx =  paper ()->note_width ()/2;
    correct = - (Real)dir_ * 2.0 * paper ()->rule_thickness ();
    mol_p->translate_axis (dx, X_AXIS);	// FIXME! ugh
  }
  
  mol_p->translate_axis (coordinate_offset_f_ + correct, axis_);

  return mol_p;
}




int
Script::compare (Script  *const&l1, Script *const&l2)
{
  return l1->specs_p_->priority_i() - l2->specs_p_->priority_i ();
}

Script::~Script ()
{
  delete specs_p_;
}
 
Script::Script (Script const&s)
  : Item (s), Staff_side(s)
{
  specs_p_ = s.specs_p_ ? s.specs_p_->clone (): 0;
  stem_l_ =s.stem_l_;
  postbreak_only_b_ = s.postbreak_only_b_;
}
  
