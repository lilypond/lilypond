/*   
  grace-align-item.cc --  implement Grace_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grace-align-item.hh"
#include "align-interface.hh"

#include "paper-column.hh"
#include "paper-def.hh"

MAKE_SCHEME_CALLBACK(Grace_align_item,before_line_breaking,1);
SCM
Grace_align_item::before_line_breaking (SCM smob)
{
  Grob*me = unsmob_grob (smob);

  SCM space = me->get_grob_property ("horizontal-space");
  me->set_grob_property ("threshold",
			gh_cons (space,
				 gh_double2scm (infinity_f)));
  dynamic_cast<Item*>(me)->column_l ()->set_grob_property ("contains-grace", SCM_BOOL_T);

  return SCM_UNSPECIFIED;
}

void
Grace_align_item::set_interface (Grob*me)
{
  me->set_interface (ly_symbol2scm ("grace-align-interface"));
  me->set_grob_property ("stacking-dir", gh_int2scm (RIGHT));
  Align_interface::set_interface(me);
  Align_interface::set_axis (me,X_AXIS);
}



bool
Grace_align_item::has_interface (Grob*m)
{
  return m&& m->has_interface (ly_symbol2scm ("grace-align-interface"));
}
