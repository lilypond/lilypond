/*
  axis-group-spanner.cc -- implement Axis_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "axis-group-spanner.hh"
#include "item.hh"
#include "p-col.hh"

/** Do stuff if we're not broken. In this case the last and first
  columns usually are pre- and postbreak respectively,
  so the items from these columns need adjusting.
  */
void
Axis_group_spanner::do_break_processing_if_unbroken()
{
  Link_array<Score_elem> elems = elem_l_arr ();
  Line_of_score *my_line = line_l();
  for (int i=0; i < elems.size(); i++) 
    {
      if (!elems[i]->line_l()) 
	{
	  Item * item_l = elems[i]->item();
	  if  (item_l
	       && item_l->breakable_b_ 
	       && item_l->break_status_i() == 0) 
	    {
	      // last two checks are paranoia
	      Item * broken_item_l = 
		item_l->find_prebroken_piece (my_line);
	      add_element (broken_item_l);
	    }
	  remove_element (elems[i]);  
	}
    }
}
void
Axis_group_spanner::do_break_processing()
{
  bool breaking_self_b = ! Spanner::line_l();
  if (!breaking_self_b)  
    {
      do_break_processing_if_unbroken();
      Spanner::do_break_processing();
      return;
    }

  break_into_pieces ();
  Link_array<Score_elem> loose_elems = elem_l_arr ();
  remove_all();
  
  for (int i=0; i < loose_elems.size(); i++) 
    {
      Score_elem * elt = loose_elems[i];
      Line_of_score *elt_line = elt->line_l();
	
      if (! elt_line)
	{
	  /* this piece doesn't know where it belongs.
	     Find out if it was broken, and use the broken remains
	     */
	  if (elt->spanner()) 
	    {
	      Spanner * sp = elt->spanner();
		
	      for (int j =0; j < broken_into_l_arr_.size(); j++) 
		{
		  Axis_group_spanner * my_broken_l
		    = (Axis_group_spanner*)broken_into_l_arr_[j];
		    
		  Spanner * broken_span_l 
		    = sp->find_broken_piece (
					     ((Score_elem*)my_broken_l)->line_l());
		    
		  if (broken_span_l) 
		    my_broken_l->add_element (broken_span_l);
		    
		}
	    }
	  else if (elt->item() 
		   && elt->item()->breakable_b_ 
		   && elt->item()->break_status_i () == 0) 
	    {
	      // broken items
	      Direction  j=LEFT;
	      do 
		{
		  Item * my_item = elt->item()->broken_to_drul_[j];
		  Line_of_score * item_line_l = my_item->line_l() ;
		  if (! item_line_l) 
		    continue;
		    
		  Axis_group_spanner * v
		    = (Axis_group_spanner*)find_broken_piece (item_line_l);
		  if (v)
		    v->add_element (my_item);
		}
	      while (flip(&j) != LEFT);
	    }
	}
      else 
	{
	  /* this piece *does* know where it belongs.
	     Put it in appropriate piece of this spanner
	     */
	  Axis_group_spanner * my_broken_l
	    = (Axis_group_spanner*)find_broken_piece (elt->line_l());
	  my_broken_l->add_element (elt);
	}
    }
  
  Spanner::do_break_processing();
}

void
Axis_group_spanner::do_print() const
{
  Axis_group_element::do_print();
}



IMPLEMENT_IS_TYPE_B2(Axis_group_spanner, Spanner, Axis_group_element);
