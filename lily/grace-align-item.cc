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

MAKE_SCHEME_CALLBACK(Grace_align_item,before_line_breaking);
SCM
Grace_align_item::before_line_breaking (SCM smob)
{
  Score_element*me = unsmob_element (smob);

  SCM space = me->get_elt_property ("horizontal-space");
  me->set_elt_property ("threshold",
			gh_cons (space,
				 gh_double2scm (infinity_f)));
  dynamic_cast<Item*>(me)->column_l ()->set_elt_property ("contains-grace", SCM_BOOL_T);

  return SCM_UNSPECIFIED;
}

void
Grace_align_item::set_interface (Score_element*me)
{
  me->set_interface (ly_symbol2scm ("grace-align-interface"));
  me->set_elt_property ("stacking-dir", gh_int2scm (RIGHT));
  Align_interface::set_interface(me);
  Align_interface::set_axis (me,X_AXIS);
}



bool
Grace_align_item::has_interface (Score_element*m)
{
  return m&& m->has_interface (ly_symbol2scm ("grace-align-interface"));
}
