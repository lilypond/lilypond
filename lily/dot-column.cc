/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <set>

#include "dots.hh"
#include "dot-column.hh"
#include "rhythmic-head.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "side-position-interface.hh"
#include "axis-group-interface.hh"
#include "stem.hh"

using std::set;

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

  if (!to_boolean (me->get_grob_property ("collision-done")))
    {
      SCM l = me->get_grob_property ("dots");
      me->set_grob_property ("collision-done", SCM_BOOL_T);
  
      do_shifts (me, l);
    }
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
  Put the dots in the spaces, close to the heads.

  This is somewhat gruesome; the problem really is

  minimize (sum_j dist (head_j, dot_j))

  over all configurations. This sounds like a messy optimization
  problem to solve.
  
*/
SCM
Dot_column::do_shifts (Grob*me, SCM l)
{
  Link_array<Grob> dots;
  while (gh_pair_p (l))
    {
      dots.push (unsmob_grob (ly_car (l)));
      l = ly_cdr (l);
    }
  
  dots.sort (compare_position);
  

  set<int> taken_posns;
  for (int i=0; i < dots.size (); i++)
    {
      Grob * d = dots[i];
      int p = int (Staff_symbol_referencer::get_position (d));

      if (Staff_symbol_referencer::on_staffline (d, p)
	  || taken_posns.find (p) != taken_posns.end ())
	{
	  int pd = p;
	  int pu = p;
	  if (Staff_symbol_referencer::on_staffline (d, p))
	    {
	      pu ++;
	      pd --;
	    }
	  
	  Direction dir =  to_dir (d->get_grob_property  ("direction"));
	  if (dir != DOWN)
	      
	  while (1)
	    {
	      if (dir != DOWN)
		{
		  if (taken_posns.find (pu) == taken_posns.end ())
		    {
		      p = pu;
		      break;
		    }
		  pu += 2;
		}
	      if (dir != UP)
		{
		  if (taken_posns.find (pd) == taken_posns.end ())
		    {
		      p = pd;
		      break;
		    }
		  pd -= 2;
		}
	    }
	  Staff_symbol_referencer::set_position (d, p);  
	}
      
      taken_posns.insert (p);
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

