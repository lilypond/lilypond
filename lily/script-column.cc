/*   
  g-script-column.cc --  implement Script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "script-column.hh"
#include "side-position-interface.hh"
#include "dimension-cache.hh"
#include "group-interface.hh"

void
Script_column::add_staff_sided (Item *i)
{
  SCM p = i->get_elt_property ("script-priority");
  if (!gh_number_p (p))
    return;
  

  Group_interface gi (this, "scripts");
  gi.add_element (i);
  
  add_dependency (i);
}

Script_column::Script_column ()
{
  set_elt_property ("scripts", SCM_EOL);  
}

static int
staff_side_compare (Item * const &i1,
		    Item * const &i2)
{
  SCM p1 = i1->get_elt_property ("script-priority");
  SCM p2 = i2->get_elt_property ("script-priority");

  return gh_scm2int (p1) - gh_scm2int (p2);
}

void
Script_column::before_line_breaking ()
{
  Drul_array<Link_array<Item> > arrs;
  Link_array<Item> staff_sided 
    = Group_interface__extract_elements (this, (Item*)0, "scripts");
				     
				     
  for (int i=0; i < staff_sided.size (); i++)
    {
      Side_position_interface st (staff_sided[i]);
      arrs[st.get_direction ()].push (staff_sided[i]);
    }

  Direction d = DOWN;
  do {
    Link_array<Item> &arr(arrs[d]);
    
    arr.sort (staff_side_compare);

    Item * last = 0;
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
}

