/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dots.hh"
#include "dot-column.hh"
#include "rhythmic-head.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "side-position-interface.hh"

void
Dot_column::add_dots (Dots *d)
{
  Group_interface gi (this, "dots");
  gi.add_element (d);

  add_dependency (d);
  add_element (d);
}

void
Dot_column::add_head (Rhythmic_head *r)
{
  if (!r->dots_l ())
    return ;

  side_position (this).add_support (r);
  add_dots (r->dots_l ());
}


int
Dot_column::compare (Dots * const &d1, Dots * const &d2)
{
  Staff_symbol_referencer_interface s1(d1);
  Staff_symbol_referencer_interface s2(d2);  
  
  return int (s1.position_f () - s2.position_f ());
}


Dot_column::Dot_column ()
{
  Group_interface gi (this, "dots");
  gi.set_interface ();
  
  directional_element (this).set (RIGHT);
  set_axes(X_AXIS,X_AXIS);
}

/*
  Will fuck up in this case.

  X.  .
  X.X .
   |X .
   |
   |
   |X .
   |
   |


   Should be smarter.
 */
void
Dot_column::do_post_processing ()
{
  Link_array<Dots> dots = Group_interface__extract_elements (this, (Dots*)0 , "dots"); 
  dots.sort (Dot_column::compare);
  
  if (dots.size () < 2)
    return;
  Slice s;
  s.set_empty ();

  Array<int> taken_posns;
  int conflicts = 0;
  for (int i=0; i < dots.size (); i++)
    {
      Real p = Staff_symbol_referencer_interface (dots[i]).position_f ();
      for (int j=0; j < taken_posns.size (); j++)
	{
	  if (taken_posns[j] == (int) p)
	    conflicts++;
	}
      taken_posns.push ((int)p);
      s.unite (Slice ((int)p,
		      (int)p));      
    }

  if (!conflicts)
    return;
  
  int  middle = s.center ();
  /*
    +1 -> off by one 
   */
  int pos = middle - dots.size () + 1;
  if (!(pos % 2))
    pos ++;			// center () rounds down.

  for (int i=0; i  <dots.size (); pos += 2, i++)
    {
      staff_symbol_referencer (dots[i]).set_position(pos);
    }
}
