/*   
  note-head-side.cc --  implement Note_head_side
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "staff-side.hh"
#include "note-head-side.hh"
void
Note_head_side::add_support (Item*head_l)
{
  Side_position_interface s (this);
  s.add_support (head_l);
}


Note_head_side::Note_head_side()
{
  Side_position_interface s(this);
  s.set_axis (X_AXIS);
  s.set_direction (LEFT);
}

bool
Note_head_side::supported_b ()const
{
  Side_position_interface s(this);
  return s.supported_b ();
}
