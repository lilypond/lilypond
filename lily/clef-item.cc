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
      set_empty(break_status_i() != 1);
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
  symbol_= t;
  if (t == "violin") {
    y_position_i_ = -2;
  }
  else if (t == "soprano") {
    symbol_="alto";
    y_position_i_ = -4;
  }
  else if (t == "alto") {
    y_position_i_ = 0;
  }
  else if (t == "tenor") {
    symbol_="alto";
    y_position_i_ = 2;
  }
  else if (t == "bass") {
    y_position_i_ = 2;
  }
}

void
Clef_item::read (Clef_engraver const &k)
{
  read (k.clef_type_str_);
}

Molecule*
Clef_item::brew_molecule_p() const
{
  String t = symbol_;
  if  (change_b_)
    t += "_change";
  Atom s = paper()->lookup_l ()->clef (t);
  Molecule*output = new Molecule (Atom (s));
  output->translate_axis (paper()->internote_f () * y_position_i_, Y_AXIS);
  return output;
}


IMPLEMENT_IS_TYPE_B1(Clef_item,Item);
