/*   
  grace-align-item.cc --  implement Grace_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grace-align-item.hh"
#include "align-interface.hh"
#include "lookup.hh"
#include "paper-column.hh"
#include "paper-def.hh"

Grace_align_item::Grace_align_item (SCM s)
  : Item (s)
{
  set_elt_property ("stacking-dir", gh_int2scm (RIGHT));
  Align_interface (this).set_interface();
  Align_interface (this).set_axis (X_AXIS);
}

/*
  TODO: cfg-able
 */
void
Grace_align_item::before_line_breaking ()
{
  Real nhw = // lookup_l ()->notehead (2, "")..extent (X_AXIS).length();
    paper_l ()->get_var ("quartwidth");
  
  set_elt_property ("threshold",
		    gh_cons (gh_double2scm (nhw* 1.5),
			     gh_double2scm (infinity_f)));
  column_l ()->set_elt_property ("contains-grace", SCM_BOOL_T);
}


void
Grace_align_item::do_add_processing ()
{
}

