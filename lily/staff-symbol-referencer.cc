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

Staff_symbol_referencer::Staff_symbol_referencer ()
{
  set_elt_property ("staff-position", gh_double2scm (0.0));
  dim_cache_[Y_AXIS]->off_callbacks_.push (callback);
}


int
Staff_symbol_referencer::lines_i () const
{
  Staff_symbol *st = staff_symbol_l ();
  return st  ?  st->no_lines_i_ : 5;
}

Staff_symbol*
Staff_symbol_referencer::staff_symbol_l () const
{
  SCM st = get_elt_property ("staff-symbol");
  return dynamic_cast<Staff_symbol* > (unsmob_element(st));
}

Real
Staff_symbol_referencer::staff_line_leading_f () const
{
  Staff_symbol * st = staff_symbol_l ();
  if (st)
    return st->staff_line_leading_f_;
  else if (pscore_l_ && paper_l ())
    paper_l ()->get_var ("interline");
 
  return 0.0;
}


Real
Staff_symbol_referencer::position_f () const
{
  Real p =0.0;
  SCM pos = get_elt_property ("staff-position");
  if (gh_number_p (pos))
    p = gh_scm2double (pos);

  Staff_symbol * st = staff_symbol_l ();
  if (st)
    {
      Score_element * c = common_refpoint (st, Y_AXIS);
      Real y = relative_coordinate (c, Y_AXIS)
	- st->relative_coordinate (c, Y_AXIS);

      p += 2.0 * y / staff_line_leading_f ();
    }
  return  p;
}



/*
  should use offset callback!
 */
Real
Staff_symbol_referencer::callback (Dimension_cache const * c)
{
  Score_element * sc = dynamic_cast<Score_element*> (c->element_l ());
  Staff_symbol_referencer * ref = dynamic_cast<Staff_symbol_referencer*> (sc);
  SCM pos = sc->get_elt_property ("staff-position");
  Real off =0.0;
  if (gh_number_p (pos))
    {
      off = gh_scm2double (pos) * ref->staff_line_leading_f () /2.0;
    }
  sc->set_elt_property ("staff-position", gh_double2scm (0.0));

  return off;
}


void
Staff_symbol_referencer::set_position (Real p)
{
  Real halfspace = staff_line_leading_f ()* 0.5;
  
  translate_axis (- halfspace * position_f (), Y_AXIS);
  Staff_symbol *st = staff_symbol_l ();
  if (st)
    translate_axis (halfspace * p, Y_AXIS);
  else
    {
      //      SCM pos = get_elt_property ("staff-position");
      set_elt_property ("staff-position",
			gh_double2scm (p));
			//			gh_double2scm (p + gh_scm2double (pos)));
    }
}

