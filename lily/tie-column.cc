/*   
  tie-column.cc --  implement Tie_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "paper-column.hh"
#include "spanner.hh"
#include "tie-column.hh"
#include "group-interface.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "rhythmic-head.hh"


/*
  tie dir depends on what Tie_column does.
*/


void
Tie_column::add_tie (Grob*me, Grob *tie)
{
  if (tie->get_parent (Y_AXIS)
      && Tie_column::has_interface (tie->get_parent (Y_AXIS)))
    return ;

   
  if (!Pointer_group_interface::count (me, "ties"))
    {
      dynamic_cast<Spanner*> (me)->set_bound (LEFT, Tie::head (tie, LEFT));
      dynamic_cast<Spanner*> (me)->set_bound (RIGHT, Tie::head (tie, RIGHT));
    }
  
   
  tie->set_parent (me, Y_AXIS);
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("ties"), tie);
  tie->add_dependency (me);
}



void
Tie_column::set_directions (Grob*me)
{
  werner_directions (me);
}

int
tie_compare (Grob* const & s1,
	     Grob* const & s2)
{
  return sign (Tie::get_position (s1) - Tie::get_position (s2));
}


  
/*
  Werner:
  
  . The algorithm to choose the direction of the ties doesn't work
  properly.  I suggest the following for applying ties sequentially
  from top to bottom:
  
  + The topmost tie is always `up'.
  
  + If there is a vertical gap to the last note above larger than
  or equal to a fifth (or sixth?), the tie is `up', otherwise it
  is `down'.
  
  + The bottommost tie is always `down'.
  
*/
void
Tie_column::werner_directions (Grob *me)
{
  Link_array<Grob> ties =
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "ties");

  if (!ties.size ())
    return ;
  
  ties.sort (tie_compare);

  Direction d = get_grob_direction (me);
  if (d)
    {
      for (int i = ties.size (); i--;)
	{
	  Grob *  t = ties[i];
	  if (!get_grob_direction (t))
	    set_grob_direction (t, d);
	}
      return ;
    }
  
  if (ties.size () == 1)
    {
      Grob *  t = ties[0];
      if (t->is_live ()
	  && !get_grob_direction (t))
	set_grob_direction (t,Tie::get_default_dir (t));
      return ;
    }

  Real last_down_pos = 10000;
  if (!get_grob_direction (ties[0]))
    set_grob_direction (ties[0], DOWN);

  /*
    Go downward.
   */
  Grob *last_tie = 0;
  for (int i = ties.size (); i--;)
    {
      Grob *t = ties[i];
      
      Direction d = get_grob_direction (t);
      Real p  = Tie::get_position (t);
      if (!d)
	{
	  if (last_tie
	      && Tie::get_column_rank (t, LEFT)
	         < Tie::get_column_rank (last_tie, LEFT))
	    {
	      d = DOWN;
	    }
	  else if (last_down_pos - p > 5)
	    {
	      d = UP;
	    }
	  else
	    {
	      d = DOWN;
	    }

	  set_grob_direction (t, d);
	}

      if (d == DOWN)
	last_down_pos = p;

      last_tie = t;
    }

  return ;
}


MAKE_SCHEME_CALLBACK (Tie_column,after_line_breaking,1);
SCM
Tie_column::after_line_breaking (SCM smob)
{
  werner_directions (unsmob_grob (smob));
  return SCM_UNSPECIFIED;
}

/*
  Extend the spanner over its Tie constituents.
 */
MAKE_SCHEME_CALLBACK (Tie_column, before_line_breaking, 1);
SCM
Tie_column::before_line_breaking (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (smob));
  for (SCM s = me->get_property ("ties"); scm_is_pair (s); s = scm_cdr (s))
    {
      Spanner *tie = dynamic_cast<Spanner*> (unsmob_grob (scm_car (s)));
      Direction dir = LEFT;
      do
	{
	  if (dir * Paper_column::get_rank (tie->get_bound (dir)->get_column ()) 
	      > dir * Paper_column::get_rank (me->get_bound (dir)->get_column ()))
	    {
	      me->set_bound (dir, Tie::head (tie, dir));
	    }
	}
      while (flip (&dir) != LEFT);
    }
  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Tie_column,"tie-column-interface",
  "Object that sets directions of multiple ties in a tied chord",
  "direction");

