/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <ctype.h>
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

/*
 * Convert input clef string to 
 * a clef symbol and a line position.
 * This would be better done in the lexer (more efficient)
 * or as a table-lookup.
 */
void
Clef_item::read (String t)
{
  symbol_= t;
  if (t == "violin") 
    {
      y_position_i_ = -2;
    }
  else if (t == "bass") 
    {
      y_position_i_ = 2;
    }
  else if (t == "G" || t == "G2" || t == "treble")
    {
      symbol_ = "violin";
      y_position_i_ = -2;
    }
  else if (t == "french" || t == "G1") 
    {
      symbol_="violin";
      y_position_i_ = -4;
    }
  else if (t == "soprano" || t == "C1") 
    {
      symbol_="alto";
      y_position_i_ = -4;
    }
  else if (t == "mezzosoprano" || t == "C2")
    {
      symbol_ = "alto";
      y_position_i_ = -2;
    }
  else if (t == "alto") 
    {
      symbol_ = "alto";
      y_position_i_ = 0;
    }
  else if (t == "C3")
    {
      symbol_ = "alto";
      y_position_i_ = 0;
  }
  else if (t == "tenor" || t == "C4") 
  {
      symbol_ = "alto";
      y_position_i_ = 2;
    }
  else if (t == "baritone" || t == "C5")
    {
      symbol_ = "alto";
      y_position_i_ = 4;
    }
  else if (t == "varbaritone" || t == "F3")
    {
      symbol_ = "bass";
      y_position_i_ = 0;
    }
  else if (t == "F" || t == "F4")
    {
      symbol_ = "bass";
      y_position_i_ = 2;
    }
  else if (t == "subbass")
    {
      symbol_ = "bass";
      y_position_i_ = 4;
    }
  else if (isdigit(t[1]))
	  switch (t[0])
	  { // we've already dealt with plain F, G  or C clef 
		  // position 0 is line 3.	  
	  case 'G':
	  case 'g':
		  symbol_ = "violin";
		  y_position_i_ =   2 * (t[1] - '0') - 6;
		  break;
	  case 'F':
	  case 'f':
		  symbol_ = "bass";
		  y_position_i_ = 2 * (t[1] - '0') - 6;
		  break;
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
