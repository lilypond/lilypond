/*   
  g-script-column.cc --  implement Script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "script-column.hh"
#include "staff-side.hh"
#include "dimension-cache.hh"


void
Script_column::add_staff_sided (Item *i)
{
  SCM p = i->get_elt_property ("script-priority");
  if (p == SCM_UNDEFINED)
    return;
  
  staff_sided_item_l_arr_.push (i);
  add_dependency (i);
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
Script_column::do_pre_processing ()
{
  Drul_array<Link_array<Item> > arrs;

  for (int i=0; i < staff_sided_item_l_arr_.size (); i++)
    {
      Staff_sidify st (staff_sided_item_l_arr_[i]);
      arrs[st.get_direction ()].push (staff_sided_item_l_arr_[i]);
    }

  Direction d = DOWN;
  do {
    Link_array<Item> &arr(arrs[d]);
    
    arr.sort (staff_side_compare);

    Item * last = 0;
    for (int i=0; i < arr.size (); i++)
      {
	Staff_sidify s (arr[i]);
	if (last)
	  {
	    s.add_support (last);
	  }
	    
	arr[i]->remove_elt_property ("script-priority");
	last = arr[i];
      }
    
  } while (flip (&d) != DOWN);
}

