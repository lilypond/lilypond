/*
  spacing-loose-columns.cc -- implement loose column spacing.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "system.hh"
#include "paper-column.hh"
#include "column-x-positions.hh"
#include "pointer-group-interface.hh"
#include "staff-spacing.hh"
#include "note-spacing.hh"
#include "spacing-spanner.hh"

#include "moment.hh"

/* Find the loose columns in POSNS, and drape them around the columns
   specified in BETWEEN-COLS.  */
void
set_loose_columns (System *which, Column_x_positions const *posns)
{
  int loose_col_count = posns->loose_cols_.size ();
  if (!loose_col_count)
    return;

  Real default_padding = 1.0;
  for (int i = 0; i < loose_col_count; i++)
    {
      int divide_over = 1;
      Item *loose = dynamic_cast<Item *> (posns->loose_cols_[i]);
      Paper_column *col = dynamic_cast<Paper_column *> (loose);

      if (col->get_system ())
	continue;

      Item *left = 0;
      Item *right = 0;

      Link_array<Item> clique;
      while (1)
	{
	  SCM between = loose->get_object ("between-cols");
	  if (!scm_is_pair (between))
	    break;

	  Item *le = dynamic_cast<Item *> (unsmob_grob (scm_car (between)));
	  Item *re = dynamic_cast<Item *> (unsmob_grob (scm_cdr (between)));

	  if (! (le && re))
	    break;

	  if (!left && le)
	    {
	      left = le->get_column ();
	      if (!left->get_system ())
		left = left->find_prebroken_piece (RIGHT);

	      clique.push_back (left);
	    }

	  clique.push_back (loose);

	  divide_over++;
	  loose = right = re->get_column ();
	}

      if (!right->get_system ())
	right = right->find_prebroken_piece (LEFT);

      clique.push_back (right);

      Grob *common = right->common_refpoint (left, X_AXIS);
      Item *finished_right_column = clique.back ();

      for (vsize j = clique.size () - 2; j > 0; j--)
	{
	  int count = 0;
	  Real total_space = 0.0;
	  Real total_fixed = 0.0;

	  extract_grob_set (col, "spacing-wishes", wishes);
	  for (vsize i = 0; i < wishes.size (); i++)
	    {
	      Grob *spacing = wishes[i];
	      Real space = 0.0;
	      Real fixed = 0.0;

	      if (Staff_spacing::has_interface (spacing))
		Staff_spacing::get_spacing_params (spacing, &space, &fixed);
	      else if (Note_spacing::has_interface (spacing))
		{
		  Spacing_options options;
		  options.init ();

		  fixed = robust_relative_extent (col, col, X_AXIS)[RIGHT];

		  Moment dt = Paper_column::when_mom (right) - Paper_column::when_mom (col);
		  bool expand;
		  space = options.get_duration_space (dt, &expand);
		  Note_spacing::get_spacing (spacing, right, space, options.increment_,
					     &space, &fixed);
		}
	      else
		continue;

	      count++;

	      total_space += space;
	      total_fixed += fixed;
	    }

	  Real distance_to_next = 0.0;
	  Real right_point = 0.0;
	  if (count)
	    {
	      total_space /= count;
	      total_fixed /= count;

	      distance_to_next = total_space;
	      right_point
		= finished_right_column->relative_coordinate (common, X_AXIS);
	    }
	  else
	    {
	      Interval my_extent = robust_relative_extent (col, col, X_AXIS);
	      distance_to_next = my_extent[RIGHT] + default_padding;
	      right_point = robust_relative_extent (finished_right_column, common, X_AXIS)[LEFT];
	    }

	  Real my_offset = right_point - distance_to_next;

	  col->system_ = which;
	  col->translate_axis (my_offset - col->relative_coordinate (common, X_AXIS), X_AXIS);

	  finished_right_column = col;
	}
    }
}

