/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dots.hh"
#include "dot-column.hh"
#include "rhythmic-head.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "side-position-interface.hh"
#include "axis-group-interface.hh"
#include "stem.hh"

/*
  TODO: let Dot_column communicate with stem via Note_column.
 */

MAKE_SCHEME_CALLBACK (Dot_column,force_shift_callback,2);
SCM
Dot_column::force_shift_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == Y_AXIS);
  me = me->get_parent (X_AXIS);
  SCM l = me->get_grob_property ("dots");
  do_shifts (l);
  return gh_double2scm (0.0);
}

MAKE_SCHEME_CALLBACK(Dot_column,side_position, 2);
SCM
Dot_column::side_position (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == X_AXIS);

  Grob * stem = unsmob_grob (me->get_grob_property ("stem"));
  if (stem
      && !Stem::get_beam (stem)
      && Stem::duration_log (stem) > 2
      && !Stem::invisible_b (stem)
      )
    {
      /*
	trigger stem end & direction calculation.

	This will add the stem to the support if a flag collision happens.
       */
      Stem::stem_end_position (stem); 
    }
  return Side_position_interface::aligned_side (element_smob, axis);
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
SCM
Dot_column::do_shifts (SCM l)
{
  Link_array<Grob> dots;
  while (gh_pair_p (l))
    {
      dots.push (unsmob_grob (ly_car (l)));
      l = ly_cdr (l);
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
      Real p = Staff_symbol_referencer::get_position (dots[i]);
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
  if (! (pos % 2))
    pos ++;			// center () rounds down.

  for (int i=0; i < dots.size (); pos += 2, i++)
    {
      Grob * d = dots[i];
      Staff_symbol_referencer::set_position (d,pos);
    }

  return SCM_UNSPECIFIED;
}



void
Dot_column::add_head (Grob * me, Grob *rh)
{
  Grob * d = unsmob_grob (rh->get_grob_property ("dot"));
  if (d)
    {
      Side_position_interface::add_support (me,rh);

      Pointer_group_interface::add_grob (me, ly_symbol2scm ("dots"), d);
      d->add_offset_callback (Dot_column::force_shift_callback_proc , Y_AXIS);
      Axis_group_interface::add_element (me, d);
    }
}




ADD_INTERFACE (Dot_column, "dot-column-interface",
  "Interface that groups dots so they form a column",
  "direction stem");

