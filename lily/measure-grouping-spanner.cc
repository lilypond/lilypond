/*   
measure-grouping-spanner.cc --  implement Measure_grouping

source file of the GNU LilyPond music typesetter

(c) 2002--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */
#include "paper-def.hh"
#include "spanner.hh"
#include "measure-grouping-spanner.hh"
#include "lookup.hh" 
#include "item.hh"

MAKE_SCHEME_CALLBACK (Measure_grouping, brew_molecule, 1);
SCM 
Measure_grouping::brew_molecule (SCM grob)
{
  Spanner * me = dynamic_cast<Spanner*> (unsmob_grob (grob));

  /*
    TODO: robustify.
   */
  SCM which = me->get_grob_property ("style");
  SCM thick = me->get_grob_property ("thickness");
  SCM height = me->get_grob_property ("height");

  Real t = me->get_paper ()->get_realvar (ly_symbol2scm ("linethickness")) *   gh_scm2double (thick); 
  Grob *common = me->get_bound(LEFT)->common_refpoint (me->get_bound (RIGHT),
						       X_AXIS);

  Interval rext = me->get_bound (RIGHT)->extent (common, X_AXIS);
  
  
  Real w =(rext.empty_b()
	   ? me->get_bound (RIGHT)->relative_coordinate (common, X_AXIS)
	   : rext[RIGHT])
    - me->get_bound (LEFT)->relative_coordinate (common, X_AXIS);

  Interval iv (0,w);

  Molecule m; 
  if (which == ly_symbol2scm ("bracket"))
    {
      m = Lookup::bracket (X_AXIS, iv, t,-gh_scm2double (height));
    }
  else if (which == ly_symbol2scm ("triangle"))
    {
      m = Lookup::triangle (iv, t, gh_scm2double (height));
    }

  m.align_to (Y_AXIS, DOWN);
  return m.smobbed_copy();
}

ADD_INTERFACE (Measure_grouping,"measure-grouping-interface",
	       "indicate groups of beats. Valid choices for 'type are 'bracket and 'triangle.",
	       "thickness style height");

  
