/*   
  axis-group-item.cc --  implement Axis_group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "axis-group-item.hh"
#include "axis-group-interface.hh"

Axis_group_item ::Axis_group_item ()
{
  axis_group (this).set_interface ();
}
