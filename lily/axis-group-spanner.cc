/*   
  axis-group-spanner.cc --  implement Axis_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "axis-group-spanner.hh"
#include "axis-group-interface.hh"

Axis_group_spanner::Axis_group_spanner()
{
  axis_group (this).set_interface ();
}
