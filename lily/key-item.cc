/*
  key-item.cc -- implement Key_item

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  keyplacement by Mats Bengtsson
*/

#include "item.hh"
#include "key-item.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"

/*
  FIXME: too much hardcoding here.
 */
const int FLAT_TOP_PITCH=2; /* fes,ges,as and bes typeset in lower octave */
const int SHARP_TOP_PITCH=4; /*  ais and bis typeset in lower octave */


/*
  FIXME: key-item should just get a list of (position, acc), and leave
  the thinking to other parties.
 */
int
Key_item::calculate_position(Grob *ki, SCM pair) 
{
  int p = gh_scm2int (gh_car (pair));
  int a = gh_scm2int (gh_cdr (pair));  
  int c0p = gh_scm2int (ki->get_grob_property ("c0-position"));
  if (to_boolean (ki->get_grob_property ("multi-octave")))
    {
      return p + c0p;
    }
  else {
    // Find the c in the range -4 through 2
    int from_bottom_pos = c0p + 4;
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
MAKE_SCHEME_CALLBACK(Key_item,brew_molecule,1);
SCM
Key_item::brew_molecule (SCM smob)
{
  Grob*me =unsmob_element (smob);


  Real inter = Staff_symbol_referencer::staff_space (me)/2.0;
  
  SCM newas = me->get_grob_property ("new-accidentals");  
  Molecule mol;
  /*
    SCM lists are stacks, so we work from right to left, ending with
    the cancellation signature.
  */
  for (SCM s = newas; gh_pair_p (s); s = gh_cdr (s))
    {
      int a = gh_scm2int (gh_cdar (s));
      Molecule m = Font_interface::get_default_font (me)->find_by_name ("accidentals-" + to_str (a));
      m.translate_axis (calculate_position(me, gh_car (s)) * inter, Y_AXIS);
      mol.add_at_edge (X_AXIS, LEFT, m, 0);
    }

  Item *it = dynamic_cast<Item*> (me) ;
  if (it->break_status_dir () != RIGHT)
    {
      SCM old = me->get_grob_property ("old-accidentals");
      /*
	Add half a space between  cancellation and key sig.

	As suggested by [Ross], p.148.
       */
      Interval x(0, inter);
      Interval y(0,0);

      mol.add_at_edge (X_AXIS, LEFT, Lookup::blank (Box(x,y)),0);
      
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
              Molecule m =Font_interface::get_default_font (me)->find_by_name ("accidentals-0");

              m.translate_axis (calculate_position (me, gh_car (old)) * inter, Y_AXIS);
              mol.add_at_edge (X_AXIS, LEFT, m,0);	
            }
        }
    }

  return mol.smobbed_copy();
}





bool
Key_item::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("key-signature-interface"));
}

void
Key_item::set_interface (Grob*m)
{
  m->set_interface (ly_symbol2scm ("key-signature-interface"));
}
