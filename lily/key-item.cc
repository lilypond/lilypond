/*
  key-item.cc -- implement Key_item

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>

  keyplacement by Mats Bengtsson
*/

#include "key-item.hh"
#include "key.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"

#include "key-grav.hh"

const int FLAT_TOP_PITCH=2; /* fes,ges,as and bes typeset in lower octave */
const int SHARP_TOP_PITCH=4; /*  ais and bis typeset in lower octave */

Key_item::Key_item (int c)
{
  breakable_b_ =true;
  default_b_ = false;
  set_c_position (c);
}

void
Key_item::read (Key_engraver const & key_grav_r)
{
  assert (!key_grav_r.key_.multi_octave_b_);
  const Array<int> &idx_arr =key_grav_r.accidental_idx_arr_; 
  for (int i = 0 ; i< idx_arr.size(); i++) 
    {
      int note = idx_arr[i];
      int acc = ((Key &) key_grav_r.key_).oct (0).acc (note);

      add (note, acc);
    }
}

void 
Key_item::set_c_position (int c0)
{
  int from_bottom_pos = c0 + 4;	// ugh
  int octaves =(from_bottom_pos / 7) +1 ;
  from_bottom_pos =(from_bottom_pos + 7*octaves)%7;
  c_position  = from_bottom_pos - 4;
}


void
Key_item::add (int p, int a)
{
  if ((a<0 && ((p>FLAT_TOP_PITCH) || (p+c_position>4)) && (p+c_position>1)) 
      ||
      (a>0 && ((p>SHARP_TOP_PITCH) || (p+c_position>5)) && (p+c_position>2))) 
    {
      p -= 7; /* Typeset below c_position */
    }
  pitch.push (p);
  acc.push (a);
}


Molecule*
Key_item::brew_molecule_p() const
{
  Molecule*output = new Molecule;
  Real inter = paper()->internote_f ();
  
  for (int i =0; i < pitch.size(); i++) 
    {
      Atom a =paper()->lookup_l ()->accidental (acc[i]);
      a.translate_axis ((c_position + pitch[i]) * inter, Y_AXIS);
      Molecule m (a);
      output->add_at_edge (X_AXIS, RIGHT, m);	
    }
  if (pitch.size()) 
    {
      Molecule m (paper()->lookup_l ()->fill (Box (
						   Interval (0, paper()->note_width ()),
						   Interval (0,0))));

      output->add_at_edge (X_AXIS, RIGHT, m);
    }
  return output;
}

IMPLEMENT_IS_TYPE_B1(Key_item,Item);

void 
Key_item::do_pre_processing()
{
  if (default_b_) 
    {
      transparent_b_ = (break_status_i() != 1);
      set_empty (transparent_b_);
    }
}
