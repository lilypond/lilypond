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
Tie_column::set_interface (Score_element*me)
{
  me->set_interface (ly_symbol2scm ("tie-column"));
  me->set_extent_callback (0, X_AXIS);
  me->set_extent_callback (0, Y_AXIS);  
}

bool
Tie_column::has_interface (Score_element*me)
{
  return  me->has_interface (ly_symbol2scm ("tie-column"));
}

void
Tie_column::add_tie (Score_element*me,Score_element *s)
{
  Pointer_group_interface g (me, "ties");
  if (!g.count ())
    {
      dynamic_cast<Spanner*> (me)->set_bound (LEFT, Tie::head (s,LEFT));
      dynamic_cast<Spanner*> (me)->set_bound (RIGHT, Tie::head (s,RIGHT));
    }
  
  Pointer_group_interface (me, "ties").add_element (s);
  s->add_dependency (me);
}


int
tie_compare (Score_element* const & s1,
	     Score_element* const & s2)
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
Tie_column::set_directions (Score_element*me)
{
  Link_array<Score_element> ties =
    Pointer_group_interface__extract_elements (me, (Score_element*)0, "ties");


  Direction d = Directional_element_interface (me).get ();

  if (d)
    {
      for (int i = ties.size (); i--;)
	{
	  Score_element *  t = ties[i];
	  Directional_element_interface (t).set (d);
	}
      return;
    }
  
  if (ties.size () == 1)
    {
      Score_element *  t = ties[0];      
      Directional_element_interface (t).set (Tie::get_default_dir (t));
      return;
    }
  
  ties.sort (tie_compare);
  Directional_element_interface tie0(ties[0]);
  tie0.set (DOWN);
  ties.del (0);
  
  Directional_element_interface tietop(ties.pop ());
  tietop.set (UP);

  for (int i=ties.size(); i--; )
    {
      Score_element *  t = ties[i];
      Real p = Tie::position_f (t);
      Direction d = (Direction) sign (p);
      if (!d)
	d = UP;
      Directional_element_interface (t).set (d);
    }
  
}

MAKE_SCHEME_CALLBACK(Tie_column,after_line_breaking);
SCM
Tie_column::after_line_breaking (SCM smob)
{
  set_directions (unsmob_element (smob));
  return SCM_UNSPECIFIED;
}
