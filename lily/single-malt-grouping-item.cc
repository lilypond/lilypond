/*   
  single-malt-grouping-item.cc --  implement Single_malt_grouping_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#include "single-malt-grouping-item.hh"
#include "p-col.hh"

Single_malt_grouping_item ::Single_malt_grouping_item()
{
  transparent_b_ = true;
}

void
Single_malt_grouping_item::add (Item* i)
{
  assert (i);
  item_l_arr_.push (i);
  add_dependency (i);
}

Interval
Single_malt_grouping_item::do_width () const
{
  Paper_column * pc = column_l ();
  Interval w;
  for (int i=0; i < item_l_arr_.size (); i++)
    {
      Item *il = item_l_arr_[i];
      assert (pc == il->column_l ());
      w.unite  (il->width () + il->relative_coordinate (pc, X_AXIS));
    }
  
  return w + (- relative_coordinate (pc, X_AXIS)); // TODO
}

IMPLEMENT_IS_TYPE_B1(Single_malt_grouping_item, Item);

void
Single_malt_grouping_item::do_substitute_dependency (Score_elem*o, Score_elem*n)
{
  if (o->item ())
    {
      item_l_arr_.unordered_substitute (o->item (),  n ? n->item () : 0);
    }
}
