/*   
  separating-group-spanner.cc --  implement Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
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
      a.push (Rod (spacing_unit_l_arr_[i], spacing_unit_l_arr_[i+1]));
      bool lb =spacing_unit_l_arr_[i]->column_l ()->breakable_b_;
      if (lb)
	{
	  Rod r((Single_malt_grouping_item*)
		       spacing_unit_l_arr_[i]->find_prebroken_piece (RIGHT),
		       spacing_unit_l_arr_[i+1]);
	  r.distance_f_ += paper ()->interline_f () *1.5;
	  a.push (r);
	}
      bool rb=spacing_unit_l_arr_[i+1]->column_l ()->breakable_b_;
      if (rb)
	{
	  a.push (Rod (spacing_unit_l_arr_[i],
		       (Single_malt_grouping_item*)
		       spacing_unit_l_arr_[i+1]->find_prebroken_piece (LEFT)));
	}
      if (lb && rb)
	{
	  Rod r((Single_malt_grouping_item*)
		       spacing_unit_l_arr_[i]->find_prebroken_piece (RIGHT),
		       (Single_malt_grouping_item*)
		       spacing_unit_l_arr_[i+1]->find_prebroken_piece (LEFT));
	  r.distance_f_ += paper ()->interline_f () *1.5;
	  a.push (r);
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

IMPLEMENT_IS_TYPE_B1(Separating_group_spanner, Spanner);

void
Separating_group_spanner::do_substitute_dependency (Score_elem*o, Score_elem*n)
{
  if (o->is_type_b (Single_malt_grouping_item::static_name ()))
    {
      Single_malt_grouping_item*ns = n ?
	(Single_malt_grouping_item*)n->item () : 0;
      spacing_unit_l_arr_.substitute ((Single_malt_grouping_item*)o->item (), ns);
    }
}

