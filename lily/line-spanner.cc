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



/*
  Warning: this thing is a cross-staff object, so it should have empty Y-dimensions.

  (If not, you risk that this is called from the staff-alignment
  routine, via molecule_extent. At this point, the staffs aren't
  separated yet, so it doesn't work cross-staff.

*/

MAKE_SCHEME_CALLBACK (Line_spanner, brew_molecule, 1);
SCM
Line_spanner::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);

  Grob *common[] = { 0, 0 };

  Item *l = spanner->get_bound (LEFT);
  Item *r = spanner->get_bound (RIGHT);  

  /*
    FIXME: should also do something sensible across line breaks.
   */
  if (l->break_status_dir () || r->break_status_dir ())
    return SCM_EOL;
  
  for (Axis a = X_AXIS;  a < NO_AXES; a = Axis (a + 1))
    {
      common[a] = l->common_refpoint (r, a);
  
    if (!common[a])
      return SCM_EOL;
    }
  
  Offset dxy ; 
  for (Axis a = X_AXIS;  a < NO_AXES; a = Axis (a + 1))
    {
      dxy[a] = r->extent (common[a], a)[LEFT] -
	l->extent (common[a], a)[RIGHT];
    }
  
  Molecule line;
  Real gap = gh_scm2double (me->get_grob_property ("gap"));

  Offset my_off(me->relative_coordinate (common[X_AXIS], X_AXIS),
		me->relative_coordinate (common[Y_AXIS], Y_AXIS) ); 
 
  Offset his_off(l->relative_coordinate (common[X_AXIS], X_AXIS),
		 l->relative_coordinate (common[Y_AXIS], Y_AXIS) ); 
 
  dxy *= (dxy.length () - 2 * gap) / dxy.length ();
  
  SCM list = Line_spanner::line_atom (me, dxy[X_AXIS], dxy[Y_AXIS]);
    
  if (list == SCM_EOL)
    return SCM_EOL;
  
  Box b (Interval (0, dxy[X_AXIS]), Interval (0, dxy[Y_AXIS]));
  
  line = Molecule (b, list);
  line.translate_axis (l->extent (l, X_AXIS).length (), X_AXIS); 
  
  
  Offset g = dxy * (gap / dxy.length ());
  line.translate (g - my_off + his_off);
      
  return line.smobbed_copy ();
}


