/*   
  arpegggio.cc -- implement Arpeggio

  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "molecule.hh"
#include "paper-def.hh"
#include "arpeggio.hh"
#include "grob.hh"
#include "stem.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "warn.hh"
#include "font-interface.hh"

bool
Arpeggio::has_interface (Grob* me)
{
  return me && me->has_interface (ly_symbol2scm ("arpeggio-interface"));
}

MAKE_SCHEME_CALLBACK (Arpeggio, brew_molecule, 1);
SCM 
Arpeggio::brew_molecule (SCM smob) 
{
  Grob *me = unsmob_grob (smob);
  
  Grob * common = me;
  for (SCM s = me->get_grob_property ("stems"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * stem =  unsmob_grob (ly_car (s));
      common =  common->common_refpoint (Staff_symbol_referencer::staff_symbol_l (stem),
				 Y_AXIS);
    }

  /*
    TODO:
    
    Using stems here is not very convenient; should store noteheads
    instead, and also put them into the support. Now we will mess up
    in vicinity of a collision.

  */
  Interval heads;
  Real my_y = me->relative_coordinate (common, Y_AXIS);
      
  for (SCM s = me->get_grob_property ("stems"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * stem = unsmob_grob (ly_car (s));
      Grob * ss = Staff_symbol_referencer::staff_symbol_l (stem);
      Interval iv =Stem::head_positions (stem);
      iv *= Staff_symbol::staff_space (ss)/2.0;
      
      heads.unite (iv + ss->relative_coordinate (common, Y_AXIS)
		   - my_y);
    }

  if (heads.empty_b ())
    {
      programming_error ("Huh? Dumb blonde encountered?");
      /*
	Nee Valerie, jij bent _niet_ dom. 
       */
      return SCM_EOL;
    }

  Direction dir = CENTER;
  if (ly_dir_p (me->get_grob_property ("arpeggio-direction")))
    {
      dir = to_dir (me->get_grob_property ("arpeggio-direction"));
    }
  
  Molecule mol;
  Font_metric *fm =Font_interface::get_default_font (me);
  Molecule squiggle = fm->find_by_name ("scripts-arpeggio");

  Real arrow_space = (dir) ? Staff_symbol_referencer::staff_space (me)  : 0.0;
  
  Real y = heads[LEFT];
  while (y < heads[RIGHT] - arrow_space)
    {
      mol.add_at_edge (Y_AXIS, UP,squiggle, 0.0);
      y+= squiggle. extent (Y_AXIS).length ();
    }
  mol.translate_axis (heads[LEFT], Y_AXIS);
  if (dir)
    mol.add_at_edge (Y_AXIS, dir,
		     fm->find_by_name ("scripts-arpeggio-arrow-" + to_str (dir)), 0.0);
  
  return mol.smobbed_copy () ;
}

/*
  We have to do a callback, because brew_molecule () triggers a
  vertical alignment if it is cross-staff.
  This callback also adds padding.
*/
MAKE_SCHEME_CALLBACK (Arpeggio, width_callback,2);
SCM
Arpeggio::width_callback (SCM smob, SCM axis)
{
  Grob * me = unsmob_grob (smob);
  Axis a = (Axis)gh_scm2int (axis);
  assert (a == X_AXIS);
  Molecule arpeggio = Font_interface::get_default_font (me)->find_by_name ("scripts-arpeggio");

  return ly_interval2scm (arpeggio.extent (X_AXIS) * 1.5);
}


ADD_INTERFACE (Arpeggio, "arpeggio-interface",
  "Functions and settings for drawing an arpeggio symbol (a wavy line left to noteheads.",
  "stems arpeggio-direction");

