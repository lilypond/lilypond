/*   
  staff-symbol-referencer.cc -- implement Staff_symbol_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include <math.h>

#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "paper-def.hh"
 
bool
Staff_symbol_referencer::has_interface (Grob*e)
{
  return unsmob_grob (e->get_grob_property ("staff-symbol"))
    || gh_number_p (e->get_grob_property ("staff-position"));
}

int
Staff_symbol_referencer::line_count (Grob*me) 
{
  Grob *st = staff_symbol_l (me);
  return st  ?  Staff_symbol::line_count (st) : 0;
}

bool
Staff_symbol_referencer::on_staffline (Grob*me)
{
  return on_staffline (me, (int) rint (position_f (me)));
}

bool
Staff_symbol_referencer::on_staffline (Grob*me, int pos)
{
  int sz = line_count (me)-1;
  return ((pos + sz) % 2) == 0;
}

Grob*
Staff_symbol_referencer::staff_symbol_l (Grob*me) 
{
  SCM st = me->get_grob_property ("staff-symbol");
  return unsmob_grob (st);
}

Real
Staff_symbol_referencer::staff_space (Grob*me) 
{
  Grob * st = staff_symbol_l (me);
  if (st)
    return Staff_symbol::staff_space (st);

  
  return 1.0;
}


Real
Staff_symbol_referencer::position_f (Grob*me) 
{
  Real p =0.0;
  Grob * st = staff_symbol_l (me);
  Grob * c = st ? me->common_refpoint (st, Y_AXIS) : 0;
  if (st && c)
    {
      Real y = me->relative_coordinate (c, Y_AXIS)
	- st->relative_coordinate (c, Y_AXIS);

      p += 2.0 * y / Staff_symbol::staff_space (st);
    }
  else
    {
      SCM pos = me->get_grob_property ("staff-position");
      if (gh_number_p (pos))
	return gh_scm2double (pos);
    }
  
  return  p;
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
      me->set_grob_property ("staff-position", gh_double2scm (0.0));
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
  Grob * st = staff_symbol_l (me);
  if (st && me->common_refpoint (st, Y_AXIS))
    {
      Real oldpos = position_f (me);
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
  half  of the height, in staff space.
 */
Real
Staff_symbol_referencer::staff_radius (Grob*me)
{
  return (line_count (me) -1) / 2;
}


int
compare_position (Grob *const  &a, Grob * const &b)
{
  return sign (Staff_symbol_referencer::position_f ((Grob*)a) - 
    Staff_symbol_referencer::position_f ((Grob*)b));
}





ADD_INTERFACE (Staff_symbol_referencer,"staff-symbol-referencer-interface",
  "Object whose Y position is meaning with reference to a staff
symbol. Objects that have this interface should include
Staff_symbol_referencer::callback in their Y-offset-callback.
",
  "staff-position");
