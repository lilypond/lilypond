/*   
  g-script-column.cc --  implement Script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "script-column.hh"
#include "staff-side.hh"
#include "dimension-cache.hh"

static Staff_side_item *
get_Staff_side (Item *i)
{
  Graphical_element *e1 = i->dim_cache_[Y_AXIS]->parent_l_->element_l ();

  return dynamic_cast<Staff_side_item*>(e1);
}

void
Script_column::add_staff_sided (Item *i)
{
  SCM p = get_Staff_side (i)->get_elt_property (script_priority_scm_sym);
  if (p == SCM_BOOL_F)
    return;
  
  staff_sided_item_l_arr_.push (i);
  add_dependency (i);
}

static int
staff_side_compare (Item * const &i1,
		    Item * const &i2)
{
  Score_element *e1 = get_Staff_side (i1);
  Score_element *e2 = get_Staff_side (i2);

  SCM p1 = e1->get_elt_property (script_priority_scm_sym);
  SCM p2 = e2->get_elt_property (script_priority_scm_sym);

  return gh_scm2int (SCM_CDR(p1)) - gh_scm2int (SCM_CDR(p2));
}

void
Script_column::do_pre_processing ()
{
  Drul_array<Link_array<Item> > arrs;

  for (int i=0; i < staff_sided_item_l_arr_.size (); i++)
    {
      Staff_side_item * ip = get_Staff_side (staff_sided_item_l_arr_[i]);
      arrs[ip->dir_].push (staff_sided_item_l_arr_[i]);
    }

  Direction d = DOWN;
  do {
    Link_array<Item> &arr(arrs[d]);
    
    arr.sort (staff_side_compare);

    Item * last = 0;
    for (int i=0; i < arr.size (); i++)
      {
	Staff_side_item * gs = get_Staff_side (arr[i]);
	if (last)
	  {
	    gs->add_support (last);
	    gs->add_support (get_Staff_side (last));
	  }
	    
	gs->remove_elt_property (script_priority_scm_sym);
	last = arr[i];
      }
    
  } while (flip (&d) != DOWN);
}
