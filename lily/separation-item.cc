/*   
  single-malt-grouping-item.cc --  implement Separation_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separation-item.hh"
#include "paper-column.hh"
#include "debug.hh"
#include "group-interface.hh"

void
Separation_item::set_interface (Score_element*s)
{
  s->set_extent_callback (SCM_EOL, X_AXIS);
  s->set_extent_callback (SCM_EOL, Y_AXIS);
}

void
Separation_item::add_item (Score_element*s,Item* i)
{
  assert (i);
  Pointer_group_interface (s).add_element (i);
  s->add_dependency (i);
}

Interval
Separation_item::my_width (Score_element *me)
{
  Item *item = dynamic_cast<Item*> (me);
  Paper_column * pc = item->column_l ();
  Interval w;
  
  for (SCM s =  me->get_elt_property ("elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      SCM elt = gh_car (s);
      if (!unsmob_element (elt))
	continue;

      Item *il = dynamic_cast<Item*> (unsmob_element (elt));
      if (pc != il->column_l ())
	{
	  /* this shouldn't happen, but let's continue anyway. */
	  programming_error (_("Separation_item:  I've been drinking too much"));
	  continue;		/*UGH UGH*/ 
	}

      if (to_boolean (il->get_elt_property ("no-spacing-rods")))
	{
	  continue;
	}

      Interval iv (il->extent (X_AXIS));
      if (!iv.empty_b ())
	{
	  Real off = il->relative_coordinate (pc, X_AXIS);
	  w.unite  (iv + off);
	}
    }

  return w;
 // add this->offset_ ? this-> relative_coordinate ()? 
}



