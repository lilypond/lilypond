/*   
  tie-column.cc --  implement Tie_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "tie-column.hh"
#include "group-interface.hh"

Tie_column::Tie_column ()
{
  set_elt_property ("ties", SCM_EOL);
}

void
Tie_column::add_tie (Score_element *s)
{
  group (s).add_element (s);
  s->add_dependency (this);
}

void
Tie_column::set_directions ()
{
  Link_array<Score_element> s =
    Group_interface__extract_elements (this, (Score_element*)0, "ties");
}

void
Tie_column::do_post_processing ()
{
  set_directions ();
}
