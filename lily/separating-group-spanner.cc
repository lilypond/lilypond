/*   
  separating-group-spanner.cc --  implement Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "single-malt-grouping-item.hh"
#include "p-col.hh"
#include "paper-def.hh"

Array<Rod>
Separating_group_spanner::get_rods () const
{
  Array<Rod> a;
  
  for (int i=0; i < spacing_unit_l_arr_.size () -1; i++)
    {
      Single_malt_grouping_item *l =spacing_unit_l_arr_[i];
      Single_malt_grouping_item *lb
	= dynamic_cast<Single_malt_grouping_item*>(l->find_prebroken_piece (RIGHT));
      Single_malt_grouping_item *r = spacing_unit_l_arr_[i+1];
      Single_malt_grouping_item *rb
	= dynamic_cast<Single_malt_grouping_item*>(r->find_prebroken_piece (LEFT));
      
      a.push (Rod (spacing_unit_l_arr_[i], spacing_unit_l_arr_[i+1]));
      if (lb)
	{
	  Rod rod(lb, r);
	  rod.distance_f_ += paper ()->interline_f () *1.5;
	  a.push (rod);
	}
      
      if (rb)
	{
	  a.push (Rod (l, rb));
	}
      if (lb && rb)
	{
	  Rod rod(lb, rb);
	  rod.distance_f_ += paper ()->interline_f () *1.5;
	  a.push (rod);
	}
    }
      
  return a;
}

void
Separating_group_spanner::add_spacing_unit (Single_malt_grouping_item*i)
{
  spacing_unit_l_arr_.push (i);
  add_dependency (i);
}

void
Separating_group_spanner::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  if (dynamic_cast<Single_malt_grouping_item *> (o))
    {
      Single_malt_grouping_item*ns = dynamic_cast<Single_malt_grouping_item *> (n);
      spacing_unit_l_arr_.substitute (dynamic_cast<Single_malt_grouping_item *> (o), ns);
    }
}

Separating_group_spanner::Separating_group_spanner()
{
  break_helper_only_b_ = true;
}
