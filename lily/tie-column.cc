/*   
  tie-column.cc --  implement Tie_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spanner.hh"
#include "tie-column.hh"
#include "group-interface.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "rhythmic-head.hh"



void
Tie_column::set_interface (Grob*me)
{
  me->set_interface (ly_symbol2scm ("tie-column-interface"));
  me->set_extent_callback (SCM_EOL, X_AXIS);
  me->set_extent_callback (SCM_EOL, Y_AXIS) ; 
}

bool
Tie_column::has_interface (Grob*me)
{
  return  me->has_interface (ly_symbol2scm ("tie-column-interface"));
}


/*
  tie dir depends on what Tie_column does.
*/

void
Tie_column::add_tie (Grob*me,Grob *s)
{
  if (!  Pointer_group_interface ::count (me, "ties"))
    {
      dynamic_cast<Spanner*> (me)->set_bound (LEFT, Tie::head (s,LEFT));
      dynamic_cast<Spanner*> (me)->set_bound (RIGHT, Tie::head (s,RIGHT));
    }
  
  Pointer_group_interface::add_element (me, "ties",s);
  s->add_dependency (me);
}


int
tie_compare (Grob* const & s1,
	     Grob* const & s2)
{
  return sign (Tie::position_f (s1) - Tie::position_f(s2));
}

/*
  See [Ross p. 138].


  In normal chord cases, the outer ties point outwards, and the
  direction of the rest is determined by their staff position.

  Ross forgets about the tie that is *on* the middle staff line. We
  assume it goes UP. (TODO: make me settable) */
void
Tie_column::set_directions (Grob*me)
{
  Link_array<Grob> ties =
    Pointer_group_interface__extract_elements (me, (Grob*)0, "ties");

  for (int i = ties.size (); i--; )
    if (Directional_element_interface::get (ties[i]))
      ties.del (i);
  

  if(!ties.size())
    return ;
  

  Direction d = Directional_element_interface::get (me);
  if (d)
    {
      for (int i = ties.size (); i--;)
	{
	  Grob *  t = ties[i];
	  Directional_element_interface::set (t, d);
	}
      return;
    }
  
  if (ties.size () == 1)
    {
      Grob *  t = ties[0];      
      Directional_element_interface::set (t,Tie::get_default_dir (t));
      return;
    }
  
  ties.sort (tie_compare);
  Directional_element_interface::set( ties[0], DOWN);
  ties.del (0);
  
  Directional_element_interface ::set(ties.pop (), UP);
  for (int i=ties.size(); i--; )
    {
      Grob *  t = ties[i];
      Real p = Tie::position_f (t);
      Direction d = (Direction) sign (p);
      if (!d)
	d = UP;
      Directional_element_interface::set (t, d);
    }
  
}

MAKE_SCHEME_CALLBACK(Tie_column,after_line_breaking,1);
SCM
Tie_column::after_line_breaking (SCM smob)
{
  set_directions (unsmob_grob (smob));
  return SCM_UNSPECIFIED;
}
