/*   
  g-script-column.cc --  implement Script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "script-column.hh"
#include "side-position-interface.hh"
#include "dimension-cache.hh"
#include "group-interface.hh"

void
Script_column::add_staff_sided (Score_element *me, Item *i)
{
  SCM p = i->get_elt_property ("script-priority");
  if (!gh_number_p (p))
    return;

  Pointer_group_interface gi (me, "scripts");
  gi.add_element (i);
  
  me->add_dependency (i);
}

static int
staff_side_compare (Score_element * const &i1,
		    Score_element * const &i2)
{
  SCM p1 = i1->get_elt_property ("script-priority");
  SCM p2 = i2->get_elt_property ("script-priority");

  return gh_scm2int (p1) - gh_scm2int (p2);
}

MAKE_SCHEME_SCORE_ELEMENT_CALLBACK(Script_column,before_line_breaking);

SCM
Script_column::before_line_breaking (SCM smob)
{
  Score_element* me = unsmob_element (smob);
  Drul_array<Link_array<Score_element> > arrs;
  Link_array<Score_element> staff_sided 
    = Pointer_group_interface__extract_elements (me, (Score_element*)0, "scripts");
				     
				     
  for (int i=0; i < staff_sided.size (); i++)
    {
      Side_position_interface st (staff_sided[i]);
      arrs[st.get_direction ()].push (staff_sided[i]);
    }

  Direction d = DOWN;
  do {
    Link_array<Score_element> &arr(arrs[d]);
    
    arr.sort (staff_side_compare);

    Score_element * last = 0;
    for (int i=0; i < arr.size (); i++)
      {
	Side_position_interface s (arr[i]);
	if (last)
	  {
	    s.add_support (last);
	  }
	    
	arr[i]->remove_elt_property ("script-priority");
	last = arr[i];
      }
    
  } while (flip (&d) != DOWN);

  return SCM_UNDEFINED;
}

