/*
  staff-symbol.cc -- implement Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "debug.hh"
#include "item.hh"
#include "staff-symbol.hh"
#include "staff-symbol-referencer.hh"
#include "spanner.hh"



MAKE_SCHEME_CALLBACK (Staff_symbol,brew_molecule,1);

SCM
Staff_symbol::brew_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner* sp = dynamic_cast<Spanner*> (me);
  Grob * common
    = sp->get_bound (LEFT)->common_refpoint (sp->get_bound (RIGHT), X_AXIS);
  
  bool paper_raggedright = to_boolean (me->paper_l ()->get_scmvar ("raggedright"));
  bool grob_raggedright = to_boolean (me->get_grob_property ("ragged-right"));
  Real width;

  SCM width_scm = me->get_grob_property ("width");
  if (gh_number_p (width_scm)) // user-defined width
    {
      width = gh_scm2double (width_scm) *
	Staff_symbol_referencer::staff_space (me);
    }
  else // determine width automatically
    {
      if (paper_raggedright && !grob_raggedright)
	{
	  // *prevent* staff symbol from being ragged right; instead, use
	  // paper variable "linewidth"
	  width = me->paper_l ()->get_var ("linewidth");
	}
      else // determine width from my own bounds
	{
	  width = sp->get_bound (RIGHT)->relative_coordinate (common , X_AXIS);
	}
    }

  // respect indentation, if any
  width -= sp->get_bound (LEFT)->relative_coordinate (common, X_AXIS);

  if (width < 0)
    {
      warning (_f ("staff symbol: indentation yields beyond end of line"));
      width = 0;
    }

  Real t = me->paper_l ()->get_var ("linethickness");
  SCM my_thick = me->get_grob_property("thickness");
  if (gh_number_p(my_thick))
    t *= gh_scm2double (my_thick);
  
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




ADD_INTERFACE (Staff_symbol,"staff-symbol-interface",
  "This spanner draws the lines of a staff.  The center (i.e. middle line
or space) is position 0.",
	       
  "ragged-right staff-space thickness line-count");

