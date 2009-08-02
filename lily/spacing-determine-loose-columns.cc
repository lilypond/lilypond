/*
  spacing-determine-loose-columns.cc -- implement Spacing_spanner
  methods that decide which columns to turn loose.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "staff-spacing.hh"

#include "spacing-options.hh"
#include "system.hh"
#include "paper-column.hh"
#include "column-x-positions.hh"
#include "pointer-group-interface.hh"
#include "spacing-interface.hh"
#include "spacing-spanner.hh"
#include "note-spacing.hh"
#include "moment.hh"
#include "grob-array.hh"
#include "break-align-interface.hh"
#include "warn.hh"

/*
  Return whether COL is fixed to its neighbors by some kind of spacing
  constraint.


  If in doubt, then we're not loose; the spacing engine should space
  for it, risking suboptimal spacing.

  (Otherwise, we might risk core dumps, and other weird stuff.)
*/
static bool
is_loose_column (Grob *l, Grob *col, Grob *r, Spacing_options const *options)
{
  if (!to_boolean (col->get_property ("allow-loose-spacing")))
    return false;
  

  if ((options->float_nonmusical_columns_
       || options->float_grace_columns_)
      && Paper_column::when_mom (col).grace_part_)
    {
      return true;
    }

  
  if (Paper_column::is_musical (col)
      || Paper_column::is_breakable (col))
    return false;

  /*
    If this column doesn't have a proper neighbor, we should really
    make it loose, but spacing it correctly is more than we can
    currently can handle.

    (this happens in the following situation:

    |
    |    clef G
    *

    |               |      ||
    |               |      ||
    O               O       ||


    the column containing the clef is really loose, and should be
    attached right to the first column, but that is a lot of work for
    such a borderline case.)

  */

  Item *r_neighbor = unsmob_item (col->get_object ("right-neighbor"));
  Item *l_neighbor = unsmob_item (col->get_object ("left-neighbor"));

  if (!l_neighbor || !r_neighbor)
    return false;

  if (l == l_neighbor && r == r_neighbor)
    return false;

  /*
    Only declare loose if the bounds make a little sense.  This means
    some cases (two isolated, consecutive clef changes) won't be
    nicely folded, but hey, then don't do that.
  */
  if (! ((Paper_column::is_musical (l_neighbor) || Paper_column::is_breakable (l_neighbor))
	 && (Paper_column::is_musical (r_neighbor) || Paper_column::is_breakable (r_neighbor))))
    return false;

  /*
    in any case, we don't want to move bar lines.
  */
  extract_grob_set (col, "elements", elts);
  for (vsize i = elts.size (); i--;)
    {
      Grob *g = elts[i];
      if (g && Break_alignment_interface::has_interface (g))
	{
	  extract_grob_set (g, "elements", gelts);
	  for (vsize j = gelts.size (); j--;)
	    {
	      Grob *h = gelts[j];

	      if (h && h->get_property ("break-align-symbol") == ly_symbol2scm ("staff-bar"))
		return false;
	    }
	}
    }

  return true;
}

void
Spacing_spanner::set_distances_for_loose_col (Grob *me, Grob *c,
					      Drul_array<Item *> next_door,
					      Spacing_options const *options)
{
  Direction d = LEFT;
  Drul_array<Real> dists (0, 0);

  do
    {
      Item *lc = dynamic_cast<Item *> ((d == LEFT) ? next_door[LEFT] : c);
      Item *rc = dynamic_cast<Item *> (d == LEFT ? c : next_door[RIGHT]);

      extract_grob_set (lc, "spacing-wishes", wishes);
      for (vsize k = wishes.size (); k--;)
	{
	  Grob *sp = wishes[k];
	  if (Spacing_interface::left_column (sp) != lc
	      || Spacing_interface::right_column (sp) != rc)
	    continue;

	  if (Note_spacing::has_interface (sp))
	    {
	      /*
		The note spacing should be taken from the musical
		columns.
	      */
	      Real base = note_spacing (me, lc, rc, options);
	      Spring spring = Note_spacing::get_spacing (sp, rc, base, options->increment_);

	      dists[d] = max (dists[d], spring.min_distance ());
	    }
	  else if (Staff_spacing::has_interface (sp))
	    {
	      Spring spring = Staff_spacing::get_spacing (sp, rc);

	      dists[d] = max (dists[d], spring.min_distance ());
	    }
	  else
	    programming_error ("Subversive spacing wish");
	}
    }
  while (flip (&d) != LEFT);

  Rod r;
  r.distance_ = dists[LEFT] + dists[RIGHT];
  r.item_drul_ = next_door;
 
  r.add_to_cols (); 
}


/*
  Remove columns that are not tightly fitting from COLS. In the
  removed columns, set 'between-cols to the columns where it is in
  between.
*/
void
Spacing_spanner::prune_loose_columns (Grob *me,
				      vector<Grob*> *cols,
				      Spacing_options *options)
{
  vector<Grob*> newcols;

  for (vsize i = 0; i < cols->size (); i++)
    {
      Grob *c = cols->at (i);

      bool loose = (i > 0 && i + 1 < cols->size ())
	&& is_loose_column (cols->at (i - 1), c, cols->at (i + 1), options);

      if (loose)
	{
	  Grob *right_neighbor = unsmob_grob (c->get_object ("right-neighbor"));
	  Grob *left_neighbor = unsmob_grob (c->get_object ("left-neighbor"));

	  /*
	    Either object can be non existent, if the score ends
	    prematurely.
	  */
	  if (!right_neighbor || !left_neighbor)
	    {
	      c->programming_error ("Cannot determine neighbors for floating column. ");
	      c->set_object ("between-cols", scm_cons (cols->at (i-1)->self_scm (),
						       cols->at (i+1)->self_scm ()));
	    }
	  else
	    {
	      c->set_object ("between-cols", scm_cons (left_neighbor->self_scm (),
						       right_neighbor->self_scm ()));


	      /*
		Set distance constraints for loose columns
	      */
	      Drul_array<Item *> next_door (dynamic_cast<Item*> (cols->at (i - 1)),
					    dynamic_cast<Item*> (cols->at (i + 1)));

	      set_distances_for_loose_col (me, c, next_door, options);
	    }
	}

      if (!loose)
	newcols.push_back (c);
    }

  *cols = newcols;
}

/*
  Set neighboring columns determined by the spacing-wishes grob property.
*/
void
Spacing_spanner::set_explicit_neighbor_columns (vector<Grob*> const &cols)
{
  for (vsize i = 0; i < cols.size (); i++)
    {
      extract_grob_set (cols[i], "spacing-wishes", wishes);
      for (vsize j = wishes.size (); j--;)
	{
	  Item *wish = dynamic_cast<Item*> (wishes[j]);
	  Item *left_col = wish->get_column ();
	  int left_rank = Paper_column::get_rank (left_col);
	  int min_right_rank = INT_MAX;

	  extract_grob_set (wish, "right-items", right_items);
	  for (vsize k = right_items.size (); k--;)
	    {
	      Item *right_col = dynamic_cast<Item*> (right_items[k])->get_column ();
	      int right_rank = Paper_column::get_rank (right_col);

	      if (right_rank < min_right_rank)
		{
		  left_col->set_object ("right-neighbor", right_col->self_scm ());
		  min_right_rank = right_rank;
		}

	      Grob *old_left_neighbor = unsmob_grob (right_col->get_object ("left-neighbor"));
	      if (!old_left_neighbor || left_rank > Paper_column::get_rank (old_left_neighbor))
		right_col->set_object ("left-neighbor", left_col->self_scm ());
	    }
	}
    }
}

/*
  Set neighboring columns that have no left/right-neighbor set
  yet. Only do breakable non-musical columns, and musical columns.
  Why only these? --jneem
*/
void
Spacing_spanner::set_implicit_neighbor_columns (vector<Grob*> const &cols)
{
  for (vsize i = 0; i < cols.size (); i++)
    {
      Item *it = dynamic_cast<Item *> (cols[i]);
      if (!Paper_column::is_breakable (it) && !Paper_column::is_musical (it))
	continue;

      if (i && !unsmob_grob (cols[i]->get_object ("left-neighbor")))
	cols[i]->set_object ("left-neighbor", cols[i-1]->self_scm ());
      if (i + 1 < cols.size () && !unsmob_grob (cols[i]->get_object ("right-neighbor")))
	cols[i]->set_object ("right-neighbor", cols[i+1]->self_scm ());
    }
}
