/*   
  grace-align-item.cc --  implement Grace_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grace-align-item.hh"
#include "lookup.hh"
#include "paper-column.hh"
#include "paper-def.hh"

Grace_align_item::Grace_align_item ()
{
  stacking_dir_ = RIGHT;
  set_axis (X_AXIS);
}
  
void
Grace_align_item::do_pre_processing ()
{
  Real nhw = // lookup_l ()->notehead (2, "").dim_[X_AXIS].length();
    paper_l ()->get_var ("quartwidth");
  
  threshold_interval_[MIN] = nhw* 1.5;
  column_l ()->set_elt_property ("contains-grace", SCM_BOOL_T);

  
  Axis_align_item::do_pre_processing ();
  Note_head_side::do_pre_processing ();

  translate_axis (-0.5* nhw, X_AXIS); // ugh.
}


void
Grace_align_item::do_add_processing ()
{
}

