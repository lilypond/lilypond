/*
  key-item.cc -- implement Key_item

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  keyplacement by Mats Bengtsson
*/

#include "key-item.hh"
#include "key.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "musical-pitch.hh"
#include "staff-symbol-referencer.hh"

const int FLAT_TOP_PITCH=2; /* fes,ges,as and bes typeset in lower octave */
const int SHARP_TOP_PITCH=4; /*  ais and bis typeset in lower octave */

Key_item::Key_item ()
{
  set_elt_property ("breakable", SCM_BOOL_T);
  set_c_position (0);
}

int
Key_item::get_c_position () const
{
  // Find the c in the range -4 through 2
  int from_bottom_pos = c0_position_ + 4;	
  from_bottom_pos = from_bottom_pos%7;
  from_bottom_pos = (from_bottom_pos + 7)%7; // Precaution to get positive.
  return from_bottom_pos - 4;
}


void 
Key_item::set_c_position (int c0)
{
  c0_position_ = c0;
}


void
Key_item::add (int p, int a)
{
  pitch_arr_.push (p);
  acc_arr_.push (a);
}

void
Key_item::add_old (int p, int a)
{
  old_pitch_arr_.push (p);
  old_acc_arr_.push (a);
}


int
Key_item::calculate_position(int p, int a) const
{
  if (to_boolean (get_elt_property ("multi-octave")))
    {
      return p + c0_position_;
    }
  else {
    if ((a<0 && ((p>FLAT_TOP_PITCH) || (p+get_c_position ()>4)) && (p+get_c_position ()>1)) 
	||
	(a>0 && ((p>SHARP_TOP_PITCH) || (p+get_c_position ()>5)) && (p+get_c_position ()>2))) 
      {
	p -= 7; /* Typeset below c_position */
      }
    /* Provide for the four cases in which there's a glitch */
    /* it's a hack, but probably not worth */
    /* the effort of finding a nicer solution. dl. */
    if (get_c_position ()==2 && a>0 && p==3)
      p -= 7;
    if (get_c_position ()==-3 && a>0 && p==-1)
      p += 7;
    if (get_c_position ()==-4 && a<0 && p==-1)
      p += 7;
    if (get_c_position ()==-2 && a<0 && p==-3)
      p += 7;
    return p + get_c_position ();
  }
}

/*
  TODO
  - space the `natural' signs wider
  - dehair this
 */
Molecule*
Key_item::do_brew_molecule_p() const
{
  Molecule*output = new Molecule;

  Staff_symbol_referencer_interface si (this);
  Real inter = si.staff_space ()/2.0;
  
  int j;
  if ((break_status_dir () == LEFT || break_status_dir () == CENTER)
      || old_pitch_arr_.size ())
    {
      for (int i =0; i < old_pitch_arr_.size(); i++) 
        {
          for (j =0; (j < pitch_arr_.size())
		 && (old_pitch_arr_[i] != pitch_arr_[j]); j++) 
	    ;
	  
          if (j == pitch_arr_.size()
	      || (old_pitch_arr_[i] == pitch_arr_[j]
		  && old_acc_arr_[i] != acc_arr_[j]))
            {
              Molecule m =lookup_l ()->afm_find ("accidentals-0");

              m.translate_axis (calculate_position(old_pitch_arr_[i], old_acc_arr_[i]) * inter, Y_AXIS);
              output->add_at_edge (X_AXIS, RIGHT, m,0);	
            }
        }

      /*
	Add half a space between  cancellation and key sig.

	As suggested by [Ross], p.148.
       */
      Interval x(0, inter);
      Interval y(0,0);

      output->add_at_edge (X_AXIS, RIGHT, lookup_l()->fill (Box(x,y)),0);
    }
 
  for (int i =0; i < pitch_arr_.size(); i++) 
    {
      Molecule m = lookup_l ()->afm_find ("accidentals-" + to_str (acc_arr_[i]));
      m.translate_axis (calculate_position(pitch_arr_[i], acc_arr_[i]) * inter, Y_AXIS);
      output->add_at_edge (X_AXIS, RIGHT, m, 0);
    }

  return output;
}




