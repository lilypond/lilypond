/*   
  direction.cc --  implement Direction
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "direction.hh"

String
direction_str (Direction d, Axis a)
{
  String s("center");
  if (a == Y_AXIS)
    {
       s =( d == UP ? "up" : "down");
    }
  else if (a == X_AXIS)
    {
      s = (d == LEFT ? "left" : "right" );
    }
  return s;
}
