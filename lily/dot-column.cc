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



void
Dot_column::add_head (Score_element * dc, Score_element *rh)
{
  Score_element * d = unsmob_element (rh->get_elt_pointer ("dot"));
  if (d)
    {
      Side_position_interface (dc).add_support (rh);

      Pointer_group_interface gi (dc, "dots");
      gi.add_element (d);
      
      d->add_offset_callback (force_shift_callback , Y_AXIS);
      Axis_group_interface (dc).add_element (d);
    }
}


int
Dot_column::compare (Score_element * const &d1, Score_element * const &d2)
{
  Staff_symbol_referencer_interface s1(d1);
  Staff_symbol_referencer_interface s2(d2);  
  
  return int (s1.position_f () - s2.position_f ());
}


void
Dot_column::set_interface (Score_element* dc)
{
  dc->set_elt_pointer  ("dots", SCM_EOL);
  Directional_element_interface (dc).set (RIGHT);
  
  Axis_group_interface (dc).set_interface ();
  Axis_group_interface (dc).set_axes(X_AXIS,X_AXIS);
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


Real
Dot_column::force_shift_callback (Score_element * dot, Axis a)
{
  assert (a == Y_AXIS);
  Score_element * me = dot->parent_l (X_AXIS);
  SCM dots = me->get_elt_pointer ("dots");
  do_shifts (dots);
  return 0.0;
}

SCM
Dot_column::do_shifts (SCM l)
{
  Link_array<Score_element> dots;
  while (gh_pair_p (l))
    {
      dots.push (unsmob_element (gh_car (l)));
      l = gh_cdr (l);
    }
  
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
      Score_element * d = dots[i];
      Staff_symbol_referencer_interface (d).set_position(pos);
    }

  return SCM_UNDEFINED;
}
