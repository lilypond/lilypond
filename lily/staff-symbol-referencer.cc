/*   
  staff-symbol-referencer.cc -- implement Staff_symbol_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include <math.h>

#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "paper-def.hh"
#include "libc-extension.hh" 


int
Staff_symbol_referencer::line_count (Grob*me) 
{
  Grob *st = get_staff_symbol (me);
  return st  ?  Staff_symbol::line_count (st) : 0;
}

bool
Staff_symbol_referencer::on_staffline (Grob*me)
{
  return on_staffline (me, (int) rint (get_position (me)));
}

bool
Staff_symbol_referencer::on_staffline (Grob*me, int pos)
{
  int sz = line_count (me)-1;
  return ((pos + sz) % 2) == 0;
}

Grob*
Staff_symbol_referencer::get_staff_symbol (Grob*me) 
{
  SCM st = me->get_grob_property ("staff-symbol");
  return unsmob_grob (st);
}

Real
Staff_symbol_referencer::staff_space (Grob*me) 
{
  Grob * st = get_staff_symbol (me);
  if (st)
    return Staff_symbol::staff_space (st);

  return 1.0;
}

Real
Staff_symbol_referencer::line_thickness (Grob*me) 
{
  Grob * st = get_staff_symbol (me);
  if (st)
    return Staff_symbol::get_line_thickness (st);

  return me->get_paper ()->get_realvar (ly_symbol2scm ("linethickness"));
}

Real
Staff_symbol_referencer::get_position (Grob*me) 
{
  Real p =0.0;
  Grob * st = get_staff_symbol (me);
  Grob * c = st ? me->common_refpoint (st, Y_AXIS) : 0;
  if (st && c)
    {
      Real y = me->relative_coordinate (c, Y_AXIS)
	- st->relative_coordinate (c, Y_AXIS);

      p += 2.0 * y / Staff_symbol::staff_space (st);
      return p;
    }

  return robust_scm2double ( me->get_grob_property ("staff-position"), p);
}


LY_DEFINE(ly_grob_staff_position,
	  "ly:grob-staff-position",
	  1, 0,0, (SCM sg),
	  "Return the Y-position of this grob relative to the staff.")
{
  Grob * g = unsmob_grob (sg);

  SCM_ASSERT_TYPE (g, sg, SCM_ARG1, __FUNCTION__, "grob");
  Real pos = Staff_symbol_referencer::get_position (g);

  if (fabs (rint (pos) -pos) < 1e-6) // ugh.
    return gh_int2scm ((int) my_round (pos));
  else
    return gh_double2scm (pos);
}


/*
  should use offset callback!
 */
MAKE_SCHEME_CALLBACK (Staff_symbol_referencer,callback,2);
SCM
Staff_symbol_referencer::callback (SCM element_smob, SCM)
{
  Grob *me = unsmob_grob (element_smob);

  SCM pos = me->get_grob_property ("staff-position");
  Real off =0.0;
  if (gh_number_p (pos))
    {
      Real space = Staff_symbol_referencer::staff_space (me);
      off = gh_scm2double (pos) * space/2.0;
      me->set_grob_property ("staff-position", gh_int2scm (0));
    }

  return gh_double2scm (off);
}

 /*
  
  This sets the position relative to the center of the staff symbol.
 
  The function is hairy, because it can be callled in two situations:

  1.  There is no staff yet; we must set staff-position

  2.  There is a staff, and perhaps someone even applied a
  translate_axis (). Then we must compensate for the translation
  
  In either case, we set a callback to be sure that our new position
  will be extracted from staff-position
  
 */
void
Staff_symbol_referencer::set_position (Grob*me,Real p)
{
  Grob * st = get_staff_symbol (me);
  if (st && me->common_refpoint (st, Y_AXIS))
    {
      Real oldpos = get_position (me);
      me->set_grob_property ("staff-position", gh_double2scm (p - oldpos));
    }
  else
    {
      me->set_grob_property ("staff-position",
			    gh_double2scm (p));

    }

  if (me->has_offset_callback_b (Staff_symbol_referencer::callback_proc, Y_AXIS))
    return ; 

  me->add_offset_callback (Staff_symbol_referencer::callback_proc, Y_AXIS);
}

/*
  Half of the height, in staff space, i.e. 2.0 for a normal staff. 
*/
Real
Staff_symbol_referencer::staff_radius (Grob*me)
{
  return (line_count (me) -1) / 2.0;
}


int
compare_position (Grob *const  &a, Grob * const &b)
{
  return sign (Staff_symbol_referencer::get_position ((Grob*)a) - 
    Staff_symbol_referencer::get_position ((Grob*)b));
}

ADD_INTERFACE (Staff_symbol_referencer,"staff-symbol-referencer-interface",
	       "Object whose Y position is meant relative to a staff "
	       "symbol. These objects usually have "
	       "Staff_symbol_referencer::callback in their Y-offset-callback. "
	       ,
	       "staff-position");
