/*   
  staff-symbol-referencer.cc -- implement Staff_symbol_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include <math.h>

#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "paper-def.hh"




void
Staff_symbol_referencer::set_interface (Score_element * e)
{
  if (!gh_number_p (e->get_elt_property ("staff-position")))
      e->set_elt_property ("staff-position", gh_double2scm (0.0));
      
  e->add_offset_callback (callback, Y_AXIS);
}

bool
Staff_symbol_referencer::has_interface (Score_element*e)
{
  return unsmob_element (e->get_elt_property ("staff-symbol"))
    || gh_number_p (e->get_elt_property ("staff-position"));
}


int
Staff_symbol_referencer::line_count (Score_element*me) 
{
  Score_element *st = staff_symbol_l (me);
  return st  ?  Staff_symbol::line_count (st) : 0;
}

Score_element*
Staff_symbol_referencer::staff_symbol_l (Score_element*me) 
{
  SCM st = me->get_elt_property ("staff-symbol");
  return unsmob_element(st);
}

Real
Staff_symbol_referencer::staff_space (Score_element*me) 
{
  Score_element * st = staff_symbol_l (me);
  if (st)
    return Staff_symbol::staff_space (st);
  else if (me->pscore_l_ && me->paper_l ())
    return me->paper_l ()->get_var ("staffspace");
 
  return 0.0;
}


Real
Staff_symbol_referencer::position_f (Score_element*me) 
{
  Real p =0.0;
  Score_element * st = staff_symbol_l (me);
  Score_element * c = st ? me->common_refpoint (st, Y_AXIS) : 0;
  if (st && c)
    {
      Real y = me->relative_coordinate (c, Y_AXIS)
	- st->relative_coordinate (c, Y_AXIS);

      p += 2.0 * y / Staff_symbol::staff_space (st);
    }
  else
    {
      SCM pos = me->get_elt_property ("staff-position");
      if (gh_number_p (pos))
	return gh_scm2double (pos);
    }
  
  return  p;
}



/*
  should use offset callback!
 */
Real
Staff_symbol_referencer::callback (Score_element * sc,Axis )
{
  Score_element* me = (Score_element*)sc; // UGH.
  
  SCM pos = sc->get_elt_property ("staff-position");
  Real off =0.0;
  if (gh_number_p (pos))
    {
      Real space = Staff_symbol_referencer::staff_space (sc);
      off = gh_scm2double (pos) * space/2.0;
    }

  me->set_elt_property ("staff-position", gh_double2scm (0.0));

  return off;
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
Staff_symbol_referencer::set_position (Score_element*me,Real p)
{
  Score_element * st = staff_symbol_l (me);
  if (st && me->common_refpoint(st, Y_AXIS))
    {
      Real oldpos = position_f (me);
      me->set_elt_property ("staff-position", gh_double2scm (p - oldpos));
    }
  else
    {
      me->set_elt_property ("staff-position",
				gh_double2scm (p));

    }

  if (me->has_offset_callback_b (callback, Y_AXIS))
    return ; 

  me->add_offset_callback (callback, Y_AXIS);
}

/*
  half  of the height, in staff space.
 */
Real
Staff_symbol_referencer::staff_radius (Score_element*me)
{
  return  (line_count (me) -1) / 2;
}


int
compare_position (Score_element *const  &a, Score_element * const &b)
{
  return sign (Staff_symbol_referencer::position_f((Score_element*)a) - 
    Staff_symbol_referencer::position_f((Score_element*)b));
}
