/*   
  grace-align-item.cc --  implement Grace_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grace-align-item.hh"
#include "lookup.hh"

Grace_align_item::Grace_align_item ()
{
  stacking_dir_ = RIGHT;
  set_axis (X_AXIS);
}
  
void
Grace_align_item::do_pre_processing ()
{
  Real nhw = lookup_l ()->notehead (2, "").dim_[X_AXIS].length();
  threshold_interval_[MIN] = nhw* 1.5;
  
  Axis_align_item::do_pre_processing ();
  Note_head_side::do_pre_processing ();

  translate_axis (-0.5* nhw, X_AXIS); // ugh.
}

void
Grace_align_item::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  Axis_align_item::do_substitute_element_pointer (o,n);
  Note_head_side::do_substitute_element_pointer( o,n);
}
