/*
  spacing-loose-columns.cc -- implement loose column spacing.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "system.hh"
#include "paper-column.hh"
#include "column-x-positions.hh"
#include "pointer-group-interface.hh"
#include "staff-spacing.hh"
#include "note-spacing.hh"
#include "spacing-spanner.hh"
#include "warn.hh"
#include "moment.hh"
#include "spacing-options.hh"

/* Find the loose columns in POSNS, and drape them around the columns
   specified in BETWEEN-COLS.  */
void
set_loose_columns (System *which, Column_x_positions const *posns)
{
  int loose_col_count = posns->loose_cols_.size ();
  if (!loose_col_count)
    return;

  for (int i = 0; i < loose_col_count; i++)
    {
      int divide_over = 1;
      Item *loose = dynamic_cast<Item *> (posns->loose_cols_[i]);
      Paper_column *col = dynamic_cast<Paper_column *> (loose);

      if (col->get_system ())
	continue;

      Item *left = 0;
      Item *right = 0;

      vector<Item*> clique;
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

      if (right->get_system ())
	; /* do nothing */
      else if (right->find_prebroken_piece (LEFT)
	       && right->find_prebroken_piece (LEFT)->get_system () == which)
	right = right->find_prebroken_piece (LEFT);
      else if (Paper_column::get_rank (which->get_bound (RIGHT)) < Paper_column::get_rank (right))
	
	right = which->get_bound (RIGHT);
      else
	{
	  clique.back ()->programming_error ("Loose column does not have right side to attach to.");
	  System *base_system = dynamic_cast<System*> (which->original ());
	  int j = Paper_column::get_rank (clique.back ()) + 1;
	  int end_rank = Paper_column::get_rank (which->get_bound (RIGHT));
	  extract_grob_set (base_system, "columns", base_cols);
	  for (; j < end_rank; j++)
	    {
	      if (base_cols[j]->get_system () == which)
		right = dynamic_cast<Item*> ((Grob*)base_cols[j]);
	    }
	}
      

      if (!right)
	{
	  programming_error ("Can't attach loose column sensibly. Attaching to end of system.");
	  right = which->get_bound (RIGHT);
	}
      Grob *common = right->common_refpoint (left, X_AXIS);

      clique.push_back (right);

      vector<Real> clique_spacing;
      clique_spacing.push_back (0.0);
      for (vsize j = 1; j + 1 < clique.size (); j ++)
	{
	  Grob *clique_col = clique[j];

	  Paper_column *loose_col = dynamic_cast<Paper_column *> (clique[j]);
	  Paper_column *next_col = dynamic_cast<Paper_column *> (clique[j + 1]);

	  Grob *spacing = unsmob_grob (clique_col->get_object ("spacing"));
	  if (Grob *grace_spacing = unsmob_grob (clique_col->get_object ("grace-spacing")))
	    {
	      spacing = grace_spacing;
	    }
	  
	  Spacing_options options;
	  if (spacing)
	    options.init_from_grob (spacing);
	  else
	    programming_error ("Column without spacing object");

	  Real base_note_space = 0.0;

	  if (Paper_column::is_musical (next_col)
	      && Paper_column::is_musical (loose_col))
	    base_note_space = Spacing_spanner::note_spacing (spacing, loose_col, next_col,
							     &options);
	  else
	    {
	      Spring spring = Spacing_spanner::standard_breakable_column_spacing (spacing,
										  loose_col, next_col,
										  &options);

	      base_note_space = spring.distance ();
	    }

	  base_note_space = max (base_note_space,
				 robust_relative_extent (loose_col, loose_col, X_AXIS)[RIGHT]
				 - robust_relative_extent (next_col, next_col, X_AXIS)[LEFT]);
	  
	  clique_spacing.push_back (base_note_space);
	}

      Real default_padding = 1.0;
      clique_spacing.push_back (default_padding);

      Real right_point = robust_relative_extent (clique.back (), common, X_AXIS)[LEFT];
      
	  
      Grob *finished_right_column = clique.back ();
      
      for (vsize j = clique.size () - 2; j > 0; j--)
	{
	  Paper_column *clique_col = dynamic_cast<Paper_column *> (clique[j]);
	  
	  right_point = finished_right_column->relative_coordinate (common, X_AXIS);

	  Real distance_to_next = clique_spacing[j];
	  
	  Real my_offset = right_point - distance_to_next;

	  clique_col->set_system (which);
	  clique_col->translate_axis (my_offset - clique_col->relative_coordinate (common, X_AXIS), X_AXIS);	  

	  finished_right_column = clique_col;
	}
 
    }
}

