/*   
  tie-column.cc --  implement Tie_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "tie-column.hh"
#include "group-interface.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "rhythmic-head.hh"

Tie_column::Tie_column (SCM s)
  : Spanner (s)
{
  set_elt_pointer ("ties", SCM_EOL);
  set_extent_callback (0, X_AXIS);
  set_extent_callback (0, Y_AXIS);  
}

void
Tie_column::add_tie (Tie *s)
{
  Pointer_group_interface g (this, "ties");
  if (!g.count ())
    {
      set_bound (LEFT, s->head (LEFT));
      set_bound (RIGHT, s->head (RIGHT));
    }
  
  Pointer_group_interface (this, "ties").add_element (s);
  s->add_dependency (this);
}


int
tie_compare (Tie* const & s1,
	     Tie* const & s2)
{
  return sign (s1->position_f () - s2->position_f());
}

/*
  See [Ross p. 138].


  In normal chord cases, the outer ties point outwards, and the
  direction of the rest is determined by their staff position.

  Ross forgets about the tie that is *on* the middle staff line. We
  assume it goes UP. (TODO: make this settable) */
void
Tie_column::set_directions ()
{
  Link_array<Tie> ties =
    Pointer_group_interface__extract_elements (this, (Tie*)0, "ties");


  Direction d = Directional_element_interface (this).get ();

  if (d)
    {
      for (int i = ties.size (); i--;)
	{
	  Tie * t = ties[i];
	  Directional_element_interface (t).set (d);
	}
      return;
    }
  
  if (ties.size () == 1)
    {
      Tie * t = ties[0];      
      Directional_element_interface (t).set (t->get_default_dir ());
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
      Tie * t = ties[i];
      Real p = t->position_f ();
      Direction d = (Direction) sign (p);
      if (!d)
	d = UP;
      Directional_element_interface (t).set (d);
    }
  
}

GLUE_SCORE_ELEMENT(Tie_column,after_line_breaking);
SCM
Tie_column::member_after_line_breaking ()
{
  set_directions ();
  return SCM_UNDEFINED;
}
