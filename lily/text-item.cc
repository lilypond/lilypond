/*
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "musical-request.hh"
#include "paper-def.hh"
#include "text-item.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"

Text_item::Text_item (General_script_def*tdef_l, int d)
{
  dir_i_ = d;
  fat_b_ = false;
  tdef_p_ = tdef_l->clone();
}

Text_item::~Text_item()
{
  delete tdef_p_;
}

void
Text_item::do_pre_processing()
{
  if (!dir_i_)
	dir_i_ = -1;
}

Interval
Text_item::symbol_height()const
{
  return tdef_p_->get_atom (paper(), dir_i_).sym_.dim.y ();
}
  
Molecule*
Text_item::brew_molecule_p() const
{
  Atom a (tdef_p_->get_atom (paper(), dir_i_));

/*
  if ( fat_b_)
	a.sym.dim.x = tdef_p_->width (paper());
	*/
  Molecule* mol_p = new Molecule (a);

  if (dir_i_<0)		// should do something better anyway.
	mol_p->translate (-mol_p->extent().y ().left , Y_AXIS);
  mol_p->translate (pos_i_ * paper()->internote_f (), Y_AXIS);
  
  return mol_p;
}


IMPLEMENT_IS_TYPE_B1(Text_item,Item);
