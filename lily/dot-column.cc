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
Dot_column::set_interface (Grob* me)
{

  Directional_element_interface::set (me, RIGHT);
  
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


MAKE_SCHEME_CALLBACK(Dot_column,force_shift_callback,2);
SCM
Dot_column::force_shift_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == Y_AXIS);
 me = me->parent_l (X_AXIS);
  SCM dots = me->get_grob_property ("dots");
  do_shifts (dots);
  return gh_double2scm (0.0);
}

SCM
Dot_column::do_shifts (SCM l)
{
  Link_array<Grob> dots;
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
      Grob * d = dots[i];
      Staff_symbol_referencer::set_position (d,pos);
    }

  return SCM_UNSPECIFIED;
}

bool
Dot_column::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("dot-column-interface"));
}


void
Dot_column::add_head (Grob * me, Grob *rh)
{
  Grob * d = unsmob_element (rh->get_grob_property ("dot"));
  if (d)
    {
      Side_position::add_support (me,rh);

      Pointer_group_interface ::add_element (me, "dots",d);
      d->add_offset_callback (Dot_column::force_shift_callback_proc , Y_AXIS);
      Axis_group_interface::add_element (me, d);
    }
}

