/*   
  separating-group-spanner.cc --  implement Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "single-malt-grouping-item.hh"

Array<Rod>
Separating_group_spanner::get_rods () const
{
  Array<Rod> a;

  for (int i=0; i < spacing_unit_l_arr_.size () -1; i++)
    {
      a.push (Rod (spacing_unit_l_arr_[i], spacing_unit_l_arr_[i+1]));    
      if (spacing_unit_l_arr_[i]->breakable_b_)
	{
	  a.push (Rod (spacing_unit_l_arr_[i]->find_prebroken_piece (RIGHT), spacing_unit_l_arr_[i+1]));
	}
      if (spacing_unit_l_arr_[i+1]->breakable_b_)
	{
	  a.push (Rod (spacing_unit_l_arr_[i], spacing_unit_l_arr_[i+1]->find_prebroken_piece (LEFT)));
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
      Single_malt_grouping_item*ns = n ? (Single_malt_grouping_item*)n->item () : 0;
      spacing_unit_l_arr_.substitute ((Single_malt_grouping_item*)o->item (), ns);
    }
}

