/*
  key-item.cc -- implement Key_item

  source file of the GNU LilyPond music typesetter

  (c) 1996--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

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

  - TODO: put this in Scheme
  
  - lots of values trivially shared (key doesn't change very
  often). Compute those once, and use that as cache for the rest.

*/
int
alteration_pos  (SCM what, int alter, int c0p)
{
  if (gh_pair_p (what))
    return gh_scm2int (gh_car (what)) * 7 + gh_scm2int (gh_cdr (what)) + c0p;

  int p = gh_scm2int (what);

  // Find the c in the range -4 through 2
  int from_bottom_pos = c0p + 4;
  from_bottom_pos = from_bottom_pos%7;
  from_bottom_pos = (from_bottom_pos + 7)%7; // Precaution to get positive.
  int c0 = from_bottom_pos - 4;

    
  if ((alter <0 && ((p>FLAT_TOP_PITCH) || (p+c0>4)) && (p+c0>1)) 
      ||
      (alter >0 && ((p>SHARP_TOP_PITCH) || (p+c0>5)) && (p+c0>2))) 
    {
      p -= 7; /* Typeset below c_position */
    }
  /* Provide for the four cases in which there's a glitch 
       it's a hack, but probably not worth  
       the effort of finding a nicer solution.
       --dl. */
  if (c0==2 && alter >0 && p==3)
    p -= 7;
  if (c0==-3 && alter>0 && p==-1)
    p += 7;
  if (c0==-4 && alter<0 && p==-1)
    p += 7;
  if (c0==-2 && alter<0 && p==-3)
    p += 7;
    
  return p + c0;
}

/*
  TODO
  - space the `natural' signs wider
 */
MAKE_SCHEME_CALLBACK (Key_item,brew_molecule,1);
SCM
Key_item::brew_molecule (SCM smob)
{
  Grob*me =unsmob_grob (smob);

  Real inter = Staff_symbol_referencer::staff_space (me)/2.0;
  
  SCM newas = me->get_grob_property ("new-accidentals");  
  Molecule mol;
  /*
    SCM lists are stacks, so we work from right to left, ending with
    the cancellation signature.
  */
  int c0p = gh_scm2int (me->get_grob_property ("c0-position"));
  for (SCM s = newas; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM what = gh_caar (s);
      int alter = gh_scm2int (gh_cdar (s));
      int pos = alteration_pos (what, alter, c0p);
      
      Molecule m = Font_interface::get_default_font (me)->find_by_name ("accidentals-" + to_str (alter));
      m.translate_axis (pos * inter, Y_AXIS);
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
      Interval x (0, inter);
      Interval y (0,0);

      mol.add_at_edge (X_AXIS, LEFT, Lookup::blank (Box (x,y)),0);

      Molecule natural;
      if (gh_pair_p (old))
	natural=Font_interface::get_default_font (me)->find_by_name ("accidentals-0");
      
      for (; gh_pair_p (old); old = gh_cdr (old))
        {
	  SCM found = scm_assoc (gh_caar (old), newas);
	  if (found == SCM_BOOL_F
	      || gh_cdr (found) != gh_cdar (old))
	    {
	      SCM what = gh_caar (old);
	      int alter = 0;
	      int pos = alteration_pos (what, alter, c0p);

	      Molecule m = natural;
              m.translate_axis (pos* inter, Y_AXIS);

	      mol.add_at_edge (X_AXIS, LEFT, m, 0);
            }
        }
    }

  return mol.smobbed_copy ();
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
