/*   
  single-malt-grouping-item.cc --  implement Single_malt_grouping_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "single-malt-grouping-item.hh"
#include "p-col.hh"
#include "debug.hh"

Single_malt_grouping_item ::Single_malt_grouping_item()
{
  set_elt_property (break_helper_only_scm_sym, SCM_BOOL_T);
  set_elt_property (transparent_scm_sym, SCM_BOOL_T);

  // this is weird! , but needed!
  set_empty (true);
}

void
Single_malt_grouping_item::add_item (Item* i)
{
  assert (i);
  item_l_arr_.push (i);
  add_dependency (i);
}

Interval
Single_malt_grouping_item::my_width () const
{
  Paper_column * pc = column_l ();
  Interval w;
  for (int i=0; i < item_l_arr_.size (); i++)
    {
      Item *il = item_l_arr_[i];
      if (pc != il->column_l ())
	{
	  /* this shouldn't happen, but let's continue anyway. */
	  warning (_("Single_malt_grouping_item: I've been drinking too much (fixme)"));
	  continue;		/*UGH UGH*/ 
	}

      Interval iv (il->extent (X_AXIS));
      if (!iv.empty_b ())
	{
	  Real off = il->relative_coordinate (pc->dim_cache_[X_AXIS], X_AXIS);
	  w.unite  (iv + off);
	}
    }

  return w;
 // add this->offset_ ? this-> relative_coordinate ()? 
}



void
Single_malt_grouping_item::do_substitute_element_pointer (Score_element*o,
							  Score_element*n)
{
  if (dynamic_cast <Item *> (o))
    {
      item_l_arr_.unordered_substitute (dynamic_cast <Item *> (o),
					dynamic_cast <Item *> (n));
    }
}

void
Single_malt_grouping_item::do_print () const
{
#ifndef NDEBUG
  for (int i=0; i < item_l_arr_.size (); i++)
    {
      DOUT << classname (item_l_arr_[i]) << ", ";
    }
#endif
}
