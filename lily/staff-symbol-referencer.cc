/*   
  staff-symbol-referencer.cc -- implement Staff_symbol_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include <math.h>

#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "paper-def.hh"
#include "dimension-cache.hh"

Staff_symbol_referencer_interface::Staff_symbol_referencer_interface (Score_element const *sc)
{
  elt_l_ = (Score_element*)sc;
}

void
Staff_symbol_referencer_interface::set_interface ()
{
  if (!gh_number_p (elt_l_->get_elt_property ("staff-position")))
      elt_l_->set_elt_property ("staff-position", gh_double2scm (0.0));
      
  elt_l_->add_offset_callback (callback, Y_AXIS);
}

bool
Staff_symbol_referencer_interface::has_interface_b ()
{
  return unsmob_element (elt_l_->get_elt_pointer ("staff-symbol"))
    || gh_number_p (elt_l_->get_elt_property ("staff-position"));
}


int
Staff_symbol_referencer_interface::line_count () const
{
  Staff_symbol *st = staff_symbol_l ();
  return st  ?  st->line_count () : 0;
}

Staff_symbol*
Staff_symbol_referencer_interface::staff_symbol_l () const
{
  SCM st = elt_l_->get_elt_pointer ("staff-symbol");
  return dynamic_cast<Staff_symbol* > (unsmob_element(st));
}

Real
Staff_symbol_referencer_interface::staff_space () const
{
  Staff_symbol * st = staff_symbol_l ();
  if (st)
    return st->staff_space ();
  else if (elt_l_->pscore_l_ && elt_l_->paper_l ())
    return elt_l_->paper_l ()->get_var ("interline");
 
  return 0.0;
}


Real
Staff_symbol_referencer_interface::position_f () const
{
  Real p =0.0;
  Staff_symbol * st = staff_symbol_l ();
  Score_element * c = st ? elt_l_->common_refpoint (st, Y_AXIS) : 0;
  if (st && c)
    {
      Real y = elt_l_->relative_coordinate (c, Y_AXIS)
	- st->relative_coordinate (c, Y_AXIS);

      p += 2.0 * y / st->staff_space ();
    }
  else
    {
      SCM pos = elt_l_->get_elt_property ("staff-position");
      if (gh_number_p (pos))
	return gh_scm2double (pos);
    }
  
  return  p;
}



/*
  should use offset callback!
 */
Real
Staff_symbol_referencer_interface::callback (Score_element const* sc,Axis )
{
  Score_element* me = (Score_element*)sc; // UGH.
  
  SCM pos = sc->get_elt_property ("staff-position");
  Real off =0.0;
  if (gh_number_p (pos))
    {
      Real space = staff_symbol_referencer (sc).staff_space ();
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
Staff_symbol_referencer_interface::set_position (Real p)
{
  Staff_symbol * st = staff_symbol_l ();
  if (st && elt_l_->common_refpoint(st, Y_AXIS))
    {
      Real oldpos = position_f ();
      elt_l_->set_elt_property ("staff-position", gh_double2scm (p - oldpos));
    }
  else
    {
      elt_l_->set_elt_property ("staff-position",
				gh_double2scm (p));

    }

  if (elt_l_->has_offset_callback_b (callback, Y_AXIS))
    return ; 

  elt_l_->add_offset_callback (callback, Y_AXIS);
}

Staff_symbol_referencer_interface
staff_symbol_referencer (Score_element const*e)
{
  return e;			// gee, I'm so smart!
}

int
compare_position (Score_element *const  &a, Score_element * const &b)
{
  Staff_symbol_referencer_interface s1(a);
  Staff_symbol_referencer_interface s2(b);      

  return sign(s1.position_f () - s2.position_f ());
}
