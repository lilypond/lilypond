/*   
  note-head-side.cc --  implement Note_head_side
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "note-head-side.hh"

void
Note_head_side::add_support (Item*head_l)
{
  if (support_l_arr_.find_l(head_l))
    return ;
  support_l_arr_.push (head_l);
  add_dependency (head_l);
}

void
Note_head_side::do_pre_processing ()
{
  Interval x_int;
  for (int i=0; i < support_l_arr_.size(); i++) 
    {
      Dimension_cache *common = 
	common_group (support_l_arr_[i], X_AXIS);

      Real x = support_l_arr_[i]->relative_coordinate (common, X_AXIS)
	- relative_coordinate (common, X_AXIS);

      x_int.unite (x + support_l_arr_[i]->extent (X_AXIS));
    }

  if (x_int.empty_b ())
    x_int = Interval(0,0);
  
  translate_axis (-extent(X_AXIS)[RIGHT] + x_int[LEFT], X_AXIS);
}

void
Note_head_side::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  if (Item* o_l = dynamic_cast <Item *> (o))
    support_l_arr_.substitute (o_l,dynamic_cast <Item *> (n));
}
