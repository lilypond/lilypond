/*
  key-item.cc -- implement Key_item

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  keyplacement by Mats Bengtsson
*/

#include "group-interface.hh" 
#include "key-item.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"

const int FLAT_TOP_PITCH=2; /* fes,ges,as and bes typeset in lower octave */
const int SHARP_TOP_PITCH=4; /*  ais and bis typeset in lower octave */

Key_item::Key_item ()
{
  set_elt_property ("breakable", SCM_BOOL_T);
  set_elt_property ("c0-position", gh_int2scm (0));

  set_elt_property ("old-accidentals", SCM_EOL);
  set_elt_property ("new-accidentals", SCM_EOL);
}

void
Key_item::add (int p, int a)
{
  SCM pair = gh_cons (gh_int2scm (p),gh_int2scm (a));
  Group_interface (this, "new-accidentals").add_thing (pair);
}

void
Key_item::add_old (int p, int a)
{
  SCM pair = gh_cons (gh_int2scm (p),gh_int2scm (a));  
  Group_interface (this, "old-accidentals").add_thing (pair);
}


int
Key_item::calculate_position(SCM pair) const
{
  int p = gh_scm2int (gh_car (pair));
  int a = gh_scm2int (gh_cdr (pair));  
  
  if (to_boolean (get_elt_property ("multi-octave")))
    {
      return p + gh_scm2int (get_elt_property ("c0-position"));
    }
  else {
    // Find the c in the range -4 through 2
    int from_bottom_pos = gh_scm2int (get_elt_property ("c0-position")) + 4;
    from_bottom_pos = from_bottom_pos%7;
    from_bottom_pos = (from_bottom_pos + 7)%7; // Precaution to get positive.
    int c0 = from_bottom_pos - 4;

    
    if ((a<0 && ((p>FLAT_TOP_PITCH) || (p+c0>4)) && (p+c0>1)) 
	||
	(a>0 && ((p>SHARP_TOP_PITCH) || (p+c0>5)) && (p+c0>2))) 
      {
	p -= 7; /* Typeset below c_position */
      }
    /* Provide for the four cases in which there's a glitch 
       it's a hack, but probably not worth  
       the effort of finding a nicer solution.
       --dl. */
    if (c0==2 && a>0 && p==3)
      p -= 7;
    if (c0==-3 && a>0 && p==-1)
      p += 7;
    if (c0==-4 && a<0 && p==-1)
      p += 7;
    if (c0==-2 && a<0 && p==-3)
      p += 7;
    
    return p + c0;
  }
}

/*
  TODO
  - space the `natural' signs wider

  
  
 */

Molecule 
Key_item::do_brew_molecule() const
{
  Molecule mol;

  Staff_symbol_referencer_interface si (this);
  Real inter = si.staff_space ()/2.0;
  
  SCM newas = get_elt_property ("new-accidentals");  

  /*
    SCM lists are stacks, so we work from right to left, ending with
    the cancellation signature.
  */
  for (SCM s = newas; gh_pair_p (s); s = gh_cdr (s))
    {
      int a = gh_scm2int (gh_cdar (s));
      Molecule m = lookup_l ()->afm_find ("accidentals-" + to_str (a));
      m.translate_axis (calculate_position(gh_car (s)) * inter, Y_AXIS);
      mol.add_at_edge (X_AXIS, LEFT, m, 0);
    }
  
  if (break_status_dir () != RIGHT)
    {
      SCM old = get_elt_property ("old-accidentals");
      /*
	Add half a space between  cancellation and key sig.

	As suggested by [Ross], p.148.
       */
      Interval x(0, inter);
      Interval y(0,0);

      mol.add_at_edge (X_AXIS, LEFT, lookup_l()->blank (Box(x,y)),0);
      
      for (; gh_pair_p (old); old = gh_cdr (old))
        {
	  SCM found = SCM_EOL;

	  /*
	    find correspondences in pitches 
	   */
          for (SCM s = newas; gh_pair_p (s); s = gh_cdr (s))
	    if (gh_caar(s) == gh_caar (old))
	      found  = gh_car (s);
		
	  if (found == SCM_EOL || gh_cdr (found) != gh_cdar (old))
	    {
              Molecule m =lookup_l ()->afm_find ("accidentals-0");

              m.translate_axis (calculate_position(gh_car(old)) * inter, Y_AXIS);
              mol.add_at_edge (X_AXIS, LEFT, m,0);	
            }
        }


    }
 

  return mol;
}




