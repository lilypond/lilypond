/*
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "musical-request.hh"
#include "paper-def.hh"
#include "text-item.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "debug.hh"

Text_item::Text_item (General_script_def* tdef_l, Direction d) 
{
  dir_ = d;
  fat_b_ = false;
  tdef_p_ = tdef_l->clone ();
}

Text_item::~Text_item ()
{
  delete tdef_p_;
}

void
Text_item::do_pre_processing ()
{
  if (!dir_)
    dir_ = DOWN;
}

Real
Text_item::get_position_f () const
{
  // uhuh, tdef/gdef?
  if ( (tdef_p_->name () != Text_def::static_name ()) 
    || ( ( (Text_def*)tdef_p_)->style_str_ != "finger"))
    return Staff_side::get_position_f ();

  if (!dir_)
    {
      warning (_ ("Text_item::get_position_f(): "
		 "somebody forgot to set my vertical direction, returning -20"));
      return -20;
    }

  Interval v = support_height ();
  // add no extra: fingers should be just above note, no?
  return v[dir_];
}

Interval
Text_item::symbol_height () const
{
  return tdef_p_->get_atom (paper (), dir_).dim_.y ();
}
  
Molecule*
Text_item::brew_molecule_p () const
{
  Atom a (tdef_p_->get_atom (paper (), dir_));


  if (fat_b_)
    a.dim_[X_AXIS] = tdef_p_->width (paper ());
  Molecule* mol_p = new Molecule (a);

  if (dir_<0)		// should do something better anyway.
    mol_p->translate_axis (-mol_p->extent ().y ().left , Y_AXIS);
  mol_p->translate_axis (y_, Y_AXIS);
  
  return mol_p;
}


IMPLEMENT_IS_TYPE_B1 (Text_item,Item);
