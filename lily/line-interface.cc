/*   
line-interface.cc --  implement Line_interface

source file of the GNU LilyPond music typesetter

(c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */

#include "line-interface.hh"
#include "molecule.hh"
#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"



Molecule
Line_interface::make_dashed_line (Real thick, Offset from, Offset to,
			     Real dash_period, Real dash_fraction)
{
  dash_fraction = (dash_fraction >? 0) <? 1.0;
  Real on = dash_fraction * dash_period + thick; 
  Real off = dash_period - on;
  
  SCM at = scm_list_n (ly_symbol2scm ("dashed-line"),
			gh_double2scm (thick), 
			gh_double2scm (on),
			gh_double2scm (off),
			gh_double2scm (to[X_AXIS] - from[X_AXIS]),
			gh_double2scm (to[Y_AXIS] - from[Y_AXIS]),
			SCM_UNDEFINED);
  
  Box box;
  box.add_point (Offset (0,0));
  box.add_point (to - from);

  box[X_AXIS].widen (thick/2);
  box[Y_AXIS].widen (thick/2);  

  Molecule m = Molecule (box, at);
  m.translate (from);
  return m;
}
/*
  TODO: read THICK from ME
 */
Molecule
Line_interface::dashed_line (Grob *me, Real thick, Offset from, Offset to)
{
  SCM type = me->get_grob_property ("style");
  if (type == ly_symbol2scm ("dotted-line")
      || type == ly_symbol2scm ("dashed-line"))
    {
      Real fraction =
	robust_scm2double (me->get_grob_property ("dash-fraction"),
			   (type == ly_symbol2scm ("dotted-line")) ? 0.0 : 0.4);

      fraction = (fraction >? 0) <? 1.0;
      Real period = Staff_symbol_referencer::staff_space (me)
	* robust_scm2double (me->get_grob_property ("dash-period"), 1.0);

      if (period < 0)
	return Molecule ();
	
      return make_dashed_line (thick, from, to, period, fraction);
    }
  else
    {
      return Lookup::line (thick, from, to);
    }
}

ADD_INTERFACE(Line_interface, "line-interface",
	      "Generic line objects. Any object using lines supports this. ",
	      "dash-period dash-fraction thickness style")
