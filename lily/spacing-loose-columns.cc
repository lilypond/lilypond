/*
  spacing-loose-columns.cc --  implement loose column spacing.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "system.hh"
#include "paper-column.hh"
#include "column-x-positions.hh"
#include "staff-spacing.hh"


/* Find the loose columns in POSNS, and drape them around the columns
   specified in BETWEEN-COLS.  */
void
set_loose_columns (System* which, Column_x_positions const *posns)
{
  int loose_col_count = posns->loose_cols_.size ();
  for (int i = 0; i < loose_col_count; i++)
    {
      int divide_over = 1;
      Item *loose = dynamic_cast<Item*> (posns->loose_cols_[i]);
      Paper_column* col = dynamic_cast<Paper_column*> (loose);
      
      if (col->system_)
	continue;
      
      Item *left = 0;
      Item *right = 0;
      while (1)
	{
	  SCM between = loose->get_property ("between-cols");
	  if (!scm_is_pair (between))
	    break;

	  Item *le = dynamic_cast<Item*> (unsmob_grob (scm_car (between)));
	  Item *re = dynamic_cast<Item*> (unsmob_grob (scm_cdr (between)));

	  if (!(le && re))
	    break;
	  
	  if (!left && le)
	    {
	      left = le->get_column ();
	      if (!left->get_system ())
		left = left->find_prebroken_piece (RIGHT);
	    }

	  divide_over++;
	  loose = right = re->get_column ();
	}

      if (!right->get_system ())
	right = right->find_prebroken_piece (LEFT);
      
      Grob *common = right->common_refpoint (left, X_AXIS);

      int count = 0;
      Real total_space = 0.0;  
      Real total_fixed = 0.0; 
      for (SCM wish = col->get_property ("spacing-wishes"); scm_is_pair (wish); wish = scm_cdr (wish))
	{
	  Grob *spacing = unsmob_grob (scm_car (wish));
	  if (Staff_spacing::has_interface (spacing))
	    {
	      Real space = 0.0;
	      Real fixed = 0.0;
	      Staff_spacing::get_spacing_params (spacing, &space, &fixed);

	      total_fixed += fixed;
	      total_space += space;
	      count ++;
	    }
	}

      Real right_point = 0.0;
      Real distance_to_next = 0.0;
      if (count)
	{
	  total_space /= count;
	  total_fixed /= count;

	  distance_to_next = total_space;
	  right_point = right->relative_coordinate (common, X_AXIS);
	}
      else
	{
	  Interval my_extent = col->extent (col, X_AXIS);
	  distance_to_next = my_extent[RIGHT] + 1.0;
	  right_point = right->extent (common, X_AXIS)[LEFT];
	}
#if 0
      Real left_point = left->extent (common, X_AXIS)[RIGHT];

      Real space_left = ((right_point - left_point) >? 0.0)
	- (my_extent.is_empty() ? 0.0 : my_extent.length ());

      Real padding = (space_left / 2) <? 1.0;
      /*
	Put it just left of the right column, with a bit of extra space 
       */
#endif
      Real my_offset = right_point - distance_to_next;

      col->system_ = which;
      col->translate_axis (my_offset - col->relative_coordinate (common, X_AXIS), X_AXIS);
    }
}

