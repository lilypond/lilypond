/*
  line-spanner.cc -- implement Line_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "molecule.hh"
#include "item.hh"
#include "spanner.hh"
#include "line-spanner.hh"
#include "paper-def.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"

SCM
Line_spanner::line_atom (Grob* me, Real dx, Real dy)
{
  SCM list = SCM_EOL;
  SCM type = me->get_grob_property ("type");
  if (gh_symbol_p (type)
      && (type == ly_symbol2scm ("line")
	  || type == ly_symbol2scm ("dashed-line")
	  || type == ly_symbol2scm ("dotted-line")))
    {
      Real staff_space = Staff_symbol_referencer::staff_space (me);
      Real thick = me->paper_l ()->get_var ("stafflinethickness");  

      SCM s = me->get_grob_property ("line-thickness");
      if (gh_number_p (s))
	thick *= gh_scm2double (s);
  
      // maybe these should be in line-thickness?
      Real length = staff_space;
      s = me->get_grob_property ("dash-length");
      if (gh_number_p (s))
	length = gh_scm2double (s) * staff_space;

      Real period = 2 * length + thick;
      s = me->get_grob_property ("dash-period");
      if (gh_number_p (s))
	period = gh_scm2double (s) * staff_space;
      
      if (type == ly_symbol2scm ("dotted-line"))
	length = thick;
	
      if (type == ly_symbol2scm ("line"))
	length = period + thick;

      Real on = length - thick;
      Real off = period - on;

      list = gh_list (ly_symbol2scm ("dashed-line"),
		      gh_double2scm (thick),
		      gh_double2scm (on),
		      gh_double2scm (off),
		      gh_double2scm (dx),
		      gh_double2scm (dy),
		      SCM_UNDEFINED);
    }
  return list;
}


MAKE_SCHEME_CALLBACK (Line_spanner, brew_molecule, 1);
SCM
Line_spanner::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);

  Grob *common[] = { 0, 0 };
  common[X_AXIS] = spanner->get_bound (LEFT)->common_refpoint (spanner->get_bound (RIGHT), X_AXIS);
  common[Y_AXIS] = spanner->get_bound (LEFT)->common_refpoint (spanner->get_bound (RIGHT), Y_AXIS);

  if (!common[X_AXIS] || !common[Y_AXIS])
    return SCM_EOL;
  
  Real dx =
    spanner->get_bound (LEFT)->relative_coordinate (common[X_AXIS], X_AXIS)
    - spanner->get_bound (RIGHT)->relative_coordinate (common[X_AXIS], X_AXIS)
    + spanner->get_bound (RIGHT)->extent (spanner->get_bound (RIGHT),
					  X_AXIS)[LEFT]
    - spanner->get_bound (LEFT)->extent (spanner->get_bound (LEFT),
					 X_AXIS)[RIGHT];
  
  Real dy =
    spanner->get_bound (LEFT)->relative_coordinate (common[Y_AXIS], Y_AXIS)
    - spanner->get_bound (RIGHT)->relative_coordinate (common[Y_AXIS], Y_AXIS)
    + spanner->get_bound (RIGHT)->extent (spanner->get_bound (RIGHT),
					  Y_AXIS).center ()
    - spanner->get_bound (LEFT)->extent (spanner->get_bound (LEFT),
					 Y_AXIS).center ();
  
  Molecule line;
  Real gap = gh_scm2double (me->get_grob_property ("gap"));
  Offset o (dx, dy);
  o *= (o.length () - 2 * gap) / o.length ();
  
  SCM list = Line_spanner::line_atom (me, o[X_AXIS], o[Y_AXIS]);
    
  if (list == SCM_EOL)
    return SCM_EOL;
  
  Box b (Interval (0, o[X_AXIS]), Interval (0, o[Y_AXIS]));
  
  line = Molecule (b, list);
  line.translate_axis (spanner->get_bound (LEFT)->extent (spanner->get_bound (LEFT), X_AXIS).length (), X_AXIS);
  Offset g = o * (gap / o.length ());
  line.translate (g);
      
  return line.smobbed_copy ();
}


