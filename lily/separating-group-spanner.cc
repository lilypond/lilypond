/*   
  separating-group-spanner.cc --  implement Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "single-malt-grouping-item.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "dimensions.hh"

static Rod
make_rod (Single_malt_grouping_item *l, Single_malt_grouping_item *r)
{
  Rod rod;
  rod.item_l_drul_[LEFT] =l;
  rod.item_l_drul_[RIGHT]=r;

  Interval li (l->my_width ());
  Interval ri (r->my_width ());
  
  if (li.empty_b () || ri.empty_b ())
    rod.distance_f_ = 0;
  else
    rod.distance_f_ = li[RIGHT] - ri[LEFT];

  return rod;
}
  

Array<Rod>
Separating_group_spanner::get_rods () const
{
  Array<Rod> a;
  
  for (int i=0; i < spacing_unit_l_arr_.size () -1; i++)
    {
      Single_malt_grouping_item *l =spacing_unit_l_arr_[i];
      Single_malt_grouping_item *lb
	= dynamic_cast<Single_malt_grouping_item*>(l->find_broken_piece (RIGHT));
      Single_malt_grouping_item *r = spacing_unit_l_arr_[i+1];
      Single_malt_grouping_item *rb
	= dynamic_cast<Single_malt_grouping_item*>(r->find_broken_piece (LEFT));
      
      a.push (make_rod(spacing_unit_l_arr_[i], spacing_unit_l_arr_[i+1]));
      if (lb)
	{
	  Rod rod(make_rod (lb, r));
	  rod.distance_f_ += padding_f_;
	  a.push (rod);
	}
      
      if (rb)
	{
	  a.push (make_rod (l, rb));
	}
      
      if (lb && rb)
	{
	  Rod rod(make_rod (lb, rb));
	  rod.distance_f_ += padding_f_;
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
Separating_group_spanner::do_substitute_element_pointer (Score_element*o,
							 Score_element*n)
{
  if (dynamic_cast<Single_malt_grouping_item *> (o))
    {
      Single_malt_grouping_item*ns = dynamic_cast<Single_malt_grouping_item *> (n);
      spacing_unit_l_arr_.substitute (dynamic_cast<Single_malt_grouping_item *> (o), ns);
    }
}

Separating_group_spanner::Separating_group_spanner()
{
  set_elt_property (break_helper_only_scm_sym, SCM_BOOL_T);
  padding_f_ =0.0;
}
