/*   
  axis-align-item.cc --  implement Axis_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "axis-align-item.hh"

Axis_align_item::Axis_align_item ()
{
}

void
Axis_align_item::do_print () const
{
  Axis_group_item::do_print ();
}
