/*
  staff-symbol.cc -- implement Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "debug.hh"
#include "item.hh"
#include "staff-symbol.hh"
#include "spanner.hh"



MAKE_SCHEME_CALLBACK (Staff_symbol,brew_molecule,1);

SCM
Staff_symbol::brew_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner* sp = dynamic_cast<Spanner*> (me);
  Grob * common
    = sp->get_bound (LEFT)->common_refpoint (sp->get_bound (RIGHT), X_AXIS);
  
  Real width =
    // right_shift     - left_shift
    + sp->get_bound (RIGHT)->relative_coordinate (common , X_AXIS)
    - sp->get_bound (LEFT)->relative_coordinate (common, X_AXIS)
    ;

  Real t = me->paper_l ()->get_var ("stafflinethickness");
  int l = Staff_symbol::line_count (me);
  
  Real height = (l-1) * staff_space (me) /2;
  Molecule m;
  for (int i=0; i < l; i++)
    {
      Molecule a =
	Lookup::filledbox (Box (Interval (0,width),
					 Interval (-t/2, t/2)));

      a.translate_axis (height - i * staff_space (me), Y_AXIS);
      m.add_molecule (a);
    }

  return m.smobbed_copy ();
}

int
Staff_symbol::steps_i (Grob*me) 
{
  return line_count (me) * 2;
}

int
Staff_symbol::line_count (Grob*me) 
{
  SCM c = me->get_grob_property ("line-count");
  if (gh_number_p (c))
    return gh_scm2int (c);
  else
    return 0;
}

Real
Staff_symbol::staff_space (Grob*me)
{
  Real ss = 1.0;
  
  SCM s = me->get_grob_property ("staff-space");
  if (gh_number_p (s))
    ss *= gh_scm2double (s);
  return ss;
}

bool
Staff_symbol::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("staff-symbol-interface"));
}
