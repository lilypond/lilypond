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
#include "axis-group-interface.hh"


// todo: dots and elements duplicate each other.
void
Dot_column::add_dots (Item *d)
{
  Pointer_group_interface gi (this, "dots");
  gi.add_element (d);

  add_dependency (d);
  Axis_group_interface (this).add_element (d);
}

void
Dot_column::add_head (Rhythmic_head *r)
{
  if (!r->dots_l ())
    return ;

  Side_position_interface (this).add_support (r);
  add_dots (r->dots_l ());
}


int
Dot_column::compare (Item * const &d1, Item * const &d2)
{
  Staff_symbol_referencer_interface s1(d1);
  Staff_symbol_referencer_interface s2(d2);  
  
  return int (s1.position_f () - s2.position_f ());
}


Dot_column::Dot_column (SCM s)
  : Item (s)
{
  Pointer_group_interface gi (this, "dots");
  gi.set_interface ();
  Directional_element_interface (this).set (RIGHT);
  
  Axis_group_interface (this).set_interface ();
  Axis_group_interface (this).set_axes(X_AXIS,X_AXIS);
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

GLUE_SCORE_ELEMENT(Dot_column,after_line_breaking);
SCM
Dot_column::member_after_line_breaking ()
{
  Link_array<Item> dots = Pointer_group_interface__extract_elements (this, (Item*)0 , "dots"); 
  dots.sort (Dot_column::compare);
  
  if (dots.size () < 2)
    return SCM_UNDEFINED;
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
  return SCM_UNDEFINED;

  
  int  middle = s.center ();
  /*
    +1 -> off by one 
   */
  int pos = middle - dots.size () + 1;
  if (!(pos % 2))
    pos ++;			// center () rounds down.

  for (int i=0; i < dots.size (); pos += 2, i++)
    {
      staff_symbol_referencer (dots[i]).set_position(pos);
    }

  return SCM_UNDEFINED;
}
