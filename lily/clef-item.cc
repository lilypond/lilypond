/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "clef-item.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "clef-grav.hh"


void
Clef_item::do_pre_processing()
{
  change_b_ = ! (break_status_i() == 1);

  if (default_b_)
    {
	empty_b_ = (break_status_i() != 1);
	transparent_b_ = (break_status_i() != 1);
    }
}

Clef_item::Clef_item()
{
  breakable_b_ =true;
  default_b_ = false;
  change_b_ = true;
  read ("violin");
}

void
Clef_item::read (String t)
{
  type_= t;
  if (type_ == "violin")
	y_off = 2;
  if (type_ == "alto")
	y_off = 4;
  if (type_ == "tenor")
	y_off = 6;
  if (type_ == "bass")
	y_off = 6;
}
void
Clef_item::read (Clef_engraver const &k)
{
  read (k.clef_type_str_);
}

Molecule*
Clef_item::brew_molecule_p()const
{
  String t = type_;
  if  (change_b_)
	t += "_change";
  Symbol s = paper()->lookup_l ()->clef (t);
  Molecule*output = new Molecule (Atom (s));
  output->translate (paper()->internote_f () * y_off, Y_AXIS);
  return output;
}


IMPLEMENT_IS_TYPE_B1(Clef_item,Item);
