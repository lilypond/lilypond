/*   
  staff-bar.cc --  implement Staff_bar
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "staff-bar.hh"

Real
Staff_bar::get_bar_size () const
{
  return 4 * staff_line_leading_f ();
}
