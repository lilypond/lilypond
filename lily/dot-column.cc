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
Dot_column::add_head (Score_element * me, Score_element *rh)
{
  Score_element * d = unsmob_element (rh->get_elt_property ("dot"));
  if (d)
    {
      Side_position::add_support (me,rh);

      Pointer_group_interface gi (me, "dots");
      gi.add_element (d);
      
      d->add_offset_callback (force_shift_callback , Y_AXIS);
      Axis_group_interface::add_element (me, d);
    }
}




void
Dot_column::set_interface (Score_element* me)
{
  me->set_elt_property  ("dots", SCM_EOL);
  Directional_element_interface (me).set (RIGHT);
  
  Axis_group_interface::set_interface (me);
  Axis_group_interface::set_axes (me, X_AXIS,X_AXIS);
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
  SCM dots = me->get_elt_property ("dots");
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
  
  dots.sort (compare_position);
  
  if (dots.size () < 2)
    return SCM_UNSPECIFIED;
  Slice s;
  s.set_empty ();

  Array<int> taken_posns;
  int conflicts = 0;
  for (int i=0; i < dots.size (); i++)
    {
      Real p = Staff_symbol_referencer::position_f (dots[i]);
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
    return SCM_UNSPECIFIED;
  
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
      Staff_symbol_referencer::set_position (d,pos);
    }

  return SCM_UNSPECIFIED;
}

bool
Dot_column::has_interface (Score_element*m)
{
  return m && m->has_interface (ly_symbol2scm ("dot-column-interface"));
}
