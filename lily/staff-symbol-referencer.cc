/*   
  staff-symbol-referencer.cc -- implement Staff_symbol_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  elt_l_->set_elt_property ("staff-position", gh_double2scm (0.0));
  elt_l_->dim_cache_[Y_AXIS]->off_callbacks_.push (callback);
}


bool
Staff_symbol_referencer_interface::has_interface_b ()
{
  return unsmob_element (elt_l_->get_elt_property ("staff-symbol"))
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
  SCM st = elt_l_->get_elt_property ("staff-symbol");
  return dynamic_cast<Staff_symbol* > (unsmob_element(st));
}

Real
Staff_symbol_referencer_interface::staff_space () const
{
  Staff_symbol * st = staff_symbol_l ();
  if (st)
    return st->staff_space ();
  else if (elt_l_->pscore_l_ && elt_l_->paper_l ())
    elt_l_->paper_l ()->get_var ("interline");
 
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
Staff_symbol_referencer_interface::callback (Dimension_cache const * c)
{
  Score_element * sc = dynamic_cast<Score_element*> (c->element_l ());

  
  SCM pos = sc->get_elt_property ("staff-position");
  Real off =0.0;
  if (gh_number_p (pos))
    {
      Real space = staff_symbol_referencer_interface (sc).staff_space ();
      off = gh_scm2double (pos) * space/2.0;
    }
  sc->set_elt_property ("staff-position", gh_double2scm (0.0));

  return off;
}


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

  Array<Offset_cache_callback> &callbacks (elt_l_->dim_cache_[Y_AXIS]->off_callbacks_);
  for (int i=0; i < callbacks.size ();i++)
    if (callbacks[i] == callback)
      return ;

  callbacks.push (callback);
}

Staff_symbol_referencer_interface
staff_symbol_referencer_interface (Score_element const*e)
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
