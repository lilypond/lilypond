/*
  key-item.cc -- implement Key_item

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  keyplacement by Mats Bengtsson
*/

#include "key-item.hh"
#include "key.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"

#include "key-engraver.hh"

const int FLAT_TOP_PITCH=2; /* fes,ges,as and bes typeset in lower octave */
const int SHARP_TOP_PITCH=4; /*  ais and bis typeset in lower octave */

Key_item::Key_item ()
{
  breakable_b_ =true;
  default_b_ = false;
  set_c_position (0);
}

void
Key_item::read (Key_engraver const & key_grav_r)
{
  multi_octave_b_ = key_grav_r.key_.multi_octave_b_;
  const Array<Musical_pitch> &idx_arr = key_grav_r.accidental_idx_arr_; 
  for (int i = 0; i < idx_arr.size(); i++) 
    {
      Musical_pitch m_l =idx_arr[i];
      if (multi_octave_b_)
	 add (m_l);
      else
	add (m_l.notename_i_, m_l.accidental_i_);
    }
  const Array<Musical_pitch> &old_idx_arr = key_grav_r.old_accidental_idx_arr_; 
  for (int i = 0 ; i< old_idx_arr.size(); i++) 
    {
      Musical_pitch m_l =old_idx_arr[i];
      if (multi_octave_b_)
	 add_old (m_l);
      else
	add_old (m_l.notename_i_, m_l.accidental_i_);
    }
}

void 
Key_item::set_c_position (int c0)
{
  c0_position = c0;
  // Find the c in the range -4 through 2
  int from_bottom_pos = c0 + 4;	
  from_bottom_pos = from_bottom_pos%7;
  from_bottom_pos = (from_bottom_pos + 7)%7; // Precaution to get positive.
  c_position  = from_bottom_pos - 4;
}


void
Key_item::add (int p, int a)
{
  pitch.push (p);
  acc.push (a);
}

void
Key_item::add (const Musical_pitch& pitch_r)
{
  pitch.push (pitch_r.steps());
  acc.push (pitch_r.accidental_i_);
}

void
Key_item::add_old (int p, int a)
{
  old_pitch.push (p);
  old_acc.push (a);
}

void
Key_item::add_old (const Musical_pitch& pitch_r)
{
  old_pitch.push (pitch_r.steps());
  old_acc.push (pitch_r.accidental_i_);
}

int
Key_item::calculate_position(int p, int a) const
{
  if (multi_octave_b_) 
    {
      return p + c0_position;
    }
  else {
    if ((a<0 && ((p>FLAT_TOP_PITCH) || (p+c_position>4)) && (p+c_position>1)) 
	||
	(a>0 && ((p>SHARP_TOP_PITCH) || (p+c_position>5)) && (p+c_position>2))) 
      {
	p -= 7; /* Typeset below c_position */
      }
    return p + c_position;
  }
}

/*
  TODO space the `natural' signs wider
 */
Molecule*
Key_item::brew_molecule_p() const
{
  Molecule*output = new Molecule;
  Real inter = paper()->internote_f ();
  
  int j;
  if ((break_status_dir_ == LEFT || break_status_dir_ == CENTER)
      || old_pitch.size ())
    {
      for (int i =0; i < old_pitch.size(); i++) 
        {
          for (j =0; (j < pitch.size()) && (old_pitch[i] != pitch[j]); j++) 
	    ;
	  
          if (j == pitch.size()
	      || (old_pitch[i] == pitch[j] && old_acc[i] != acc[j]))
            {
              Atom a =lookup_l ()->accidental (0);
              a.translate_axis (calculate_position(old_pitch[i], old_acc[i]) * inter, Y_AXIS);
              Molecule m (a);
              output->add_at_edge (X_AXIS, RIGHT, m);	
            }
        }

      /*
	Add half a space between  cancellation and key sig.

	As suggested by [Ross], p.148.
       */
      Interval x(0, inter);
      Interval y(0,0);

      output->add_at_edge (X_AXIS, RIGHT, lookup_l()->fill (Box(x,y)));
    }
 
  for (int i =0; i < pitch.size(); i++) 
    {
      Atom a =lookup_l ()->accidental (acc[i]);
      a.translate_axis (calculate_position(pitch[i], acc[i]) * inter, Y_AXIS);
      Molecule m (a);
      output->add_at_edge (X_AXIS, RIGHT, m);	
    }
  if (pitch.size()) 
    {
      Molecule m (lookup_l ()->fill (Box (
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
      transparent_b_ = (break_status_dir() != RIGHT);
      set_empty (transparent_b_);
    }
}
