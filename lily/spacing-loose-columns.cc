/*
  spacing-loose-columns.cc -- implement loose column spacing.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/


#include "system.hh"
#include "paper-column.hh"
#include "column-x-positions.hh"
#include "staff-spacing.hh"
#include "pointer-group-interface.hh"
#include "spacing-spanner.hh"
#include "note-spacing.hh"

#include "break-align-interface.hh"

/* Find the loose columns in POSNS, and drape them around the columns
   specified in BETWEEN-COLS.  */
void
set_loose_columns (System *which, Column_x_positions const *posns)
{
  int loose_col_count = posns->loose_cols_.size ();
  for (int i = 0; i < loose_col_count; i++)
    {
      int divide_over = 1;
      Item *loose = dynamic_cast<Item *> (posns->loose_cols_[i]);
      Paper_column *col = dynamic_cast<Paper_column *> (loose);

      if (col->get_system ())
	continue;

      Item *left = 0;
      Item *right = 0;
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

      extract_grob_set (col, "spacing-wishes", wishes);
      for (int i = 0; i < wishes.size (); i++)
	{
	  Grob *spacing = wishes[i];
	  if (Staff_spacing::has_interface (spacing))
	    {
	      Real space = 0.0;
	      Real fixed = 0.0;
	      Staff_spacing::get_spacing_params (spacing, &space, &fixed);

	      total_fixed += fixed;
	      total_space += space;
	      count++;
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

      Real my_offset = right_point - distance_to_next;

      col->system_ = which;
      col->translate_axis (my_offset - col->relative_coordinate (common, X_AXIS), X_AXIS);
    }
}



/*
  Return whether COL is fixed to its neighbors by some kind of spacing
  constraint.


  If in doubt, then we're not loose; the spacing engine should space
  for it, risking suboptimal spacing.

  (Otherwise, we might risk core dumps, and other weird stuff.)
*/
static bool
loose_column (Grob *l, Grob *c, Grob *r)
{
  extract_grob_set (c, "right-neighbors", rns);
  extract_grob_set (c, "left-neighbors", lns);
  
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
  if (lns.is_empty () || rns.is_empty ())
    return false;

  Item *l_neighbor = dynamic_cast<Item *> (lns[0]);
  Item *r_neighbor = dynamic_cast<Item *> (rns[0]);

  if (!l_neighbor || !r_neighbor)
    return false;

  l_neighbor = l_neighbor->get_column ();
  r_neighbor = dynamic_cast<Item *> (Note_spacing::right_column (r_neighbor));

  if (l == l_neighbor && r == r_neighbor)
    return false;

  if (!l_neighbor || !r_neighbor)
    return false;

  /*
    Only declare loose if the bounds make a little sense.  This means
    some cases (two isolated, consecutive clef changes) won't be
    nicely folded, but hey, then don't do that.
  */
  if (! ((Paper_column::is_musical (l_neighbor) || Item::is_breakable (l_neighbor))
	 && (Paper_column::is_musical (r_neighbor) || Item::is_breakable (r_neighbor))))
    {
      return false;
    }

  /*
    A rather hairy check, but we really only want to move around
    clefs. (anything else?)

    in any case, we don't want to move bar lines.
  */
  extract_grob_set (c, "elements", elts);
  for (int i = elts.size (); i--; )
    {
      Grob *g = elts[i];
      if (g && Break_align_interface::has_interface (g))
	{
	  extract_grob_set (g, "elements", gelts);
	  for (int j = gelts.size (); j--; )
	    {
	      Grob *h = gelts[j];

	      /*
		ugh. -- fix staff-bar name?
	      */
	      if (h && h->get_property ("break-align-symbol") == ly_symbol2scm ("staff-bar"))
		return false;
	    }
	}
    }

  return true;
}

/*
  Remove columns that are not tightly fitting from COLS. In the
  removed columns, set 'between-cols to the columns where it is in
  between.
*/
void
Spacing_spanner::prune_loose_columns (Grob *me, Link_array<Grob> *cols,
				      Spacing_options const *options)
{
  Link_array<Grob> newcols;
  Real increment = robust_scm2double (me->get_property ("spacing-increment"), 1.2);
  for (int i = 0; i < cols->size (); i++)
    {
      if (Item::is_breakable (cols->elem (i))
	  || Paper_column::is_musical (cols->elem (i)))
	{
	  newcols.push (cols->elem (i));
	  continue;
	}

      Grob *c = cols->elem (i);
      if (loose_column (cols->elem (i - 1), c, cols->elem (i + 1)))
	{
	  extract_grob_set (c, "right-neighbors", rns_arr);
	  extract_grob_set (c, "left-neighbors", lns_arr);
	  
	  SCM lns = lns_arr.size () ? lns_arr.top()->self_scm () : SCM_BOOL_F;
	  SCM rns = rns_arr.size () ? rns_arr.top()->self_scm () : SCM_BOOL_F;
	  
	  /*
	    Either object can be non existent, if the score ends
	    prematurely.
	  */

	  extract_grob_set (unsmob_grob (rns), "right-items", right_items);
	  c->set_object ("between-cols", scm_cons (lns,
						   right_items[0]->self_scm ()));

	  /*
	    Set distance constraints for loose columns
	  */
	  Drul_array<Grob *> next_door;
	  next_door[LEFT] = cols->elem (i - 1);
	  next_door[RIGHT] = cols->elem (i + 1);
	  Direction d = LEFT;
	  Drul_array<Real> dists (0, 0);

	  do
	    {
	      dists[d] = 0.0;
	      Item *lc = dynamic_cast<Item *> ((d == LEFT) ? next_door[LEFT] : c);
	      Item *rc = dynamic_cast<Item *> (d == LEFT ? c : next_door[RIGHT]);


	      extract_grob_set (lc, "spacing-wishes", wishes);
	      for (int k = wishes.size(); k--;)
		{
		  Grob *sp = wishes[k];
		  if (Note_spacing::left_column (sp) != lc
		      || Note_spacing::right_column (sp) != rc)
		    continue;

		  Real space, fixed;
		  fixed = 0.0;
		  bool dummy;

		  if (d == LEFT)
		    {
		      /*
			The note spacing should be taken from the musical
			columns.

		      */
		      Real base = note_spacing (me, lc, rc, options, &dummy);
		      Note_spacing::get_spacing (sp, rc, base, increment, &space, &fixed);

		      space -= increment;

		      dists[d] = max (dists[d], space);
		    }
		  else
		    {
		      Real space, fixed_space;
		      Staff_spacing::get_spacing_params (sp,
							 &space, &fixed_space);

		      dists[d] = max (dists[d], fixed_space);
		    }
		}
	    }
	  while (flip (&d) != LEFT);

	  Rod r;
	  r.distance_ = dists[LEFT] + dists[RIGHT];
	  r.item_drul_[LEFT] = dynamic_cast<Item *> (cols->elem (i - 1));
	  r.item_drul_[RIGHT] = dynamic_cast<Item *> (cols->elem (i + 1));

	  r.add_to_cols ();
	}
      else
	{
	  newcols.push (c);
	}
    }

  *cols = newcols;
}

/*
  Set neighboring columns determined by the spacing-wishes grob property.
*/
void
Spacing_spanner::set_explicit_neighbor_columns (Link_array<Grob> const &cols)
{
  for (int i = 0; i < cols.size (); i++)
    {
      SCM right_neighbors = Grob_array::make_array ();
      Grob_array *rn_arr = unsmob_grob_array (right_neighbors);
      int min_rank = 100000;	// inf.

      extract_grob_set (cols[i], "spacing-wishes", wishes);
      for (int k = wishes.size(); k--;)
	{
	  Item *wish = dynamic_cast<Item *> ( wishes[k]);

	  Item *lc = wish->get_column ();
	  Grob *right = Note_spacing::right_column (wish);

	  if (!right)
	    continue;

	  Item *rc = dynamic_cast<Item *> (right);

	  int right_rank = Paper_column::get_rank (rc);
	  int left_rank = Paper_column::get_rank (lc);

	  /*
	    update the left column.
	  */
	  if (right_rank <= min_rank)
	    {
	      if (right_rank < min_rank)
		rn_arr->clear ();

	      min_rank = right_rank;
	      rn_arr->add (wish);
	    }

	  /*
	    update the right column of the wish.
	  */
	  int maxrank = 0;

	  extract_grob_set (rc, "left-neighbors", lns_arr);
	  if (lns_arr.size ())
	    {
	      Item *it = dynamic_cast<Item *> (lns_arr.top());
	      maxrank = Paper_column::get_rank (it->get_column ());
	    }

	  if (left_rank >= maxrank)
	    {
	      
	      if (left_rank > maxrank)
		{
		  Grob_array *ga = unsmob_grob_array (rc->get_object ("left-neighbors"));
		  if (ga)
		    ga->clear ();
		}

	      Pointer_group_interface::add_grob (rc, ly_symbol2scm ("left-neighbors"), wish);
	    }
	}

      if (rn_arr->size ())
	{
	  cols[i]->set_object ("right-neighbors", right_neighbors);
	}
    }
}

/*
  Set neighboring columns that have no left/right-neighbor set
  yet. Only do breakable non-musical columns, and musical columns.
*/
void
Spacing_spanner::set_implicit_neighbor_columns (Link_array<Grob> const &cols)
{
  for (int i = 0; i < cols.size (); i++)
    {
      Item *it = dynamic_cast<Item *> (cols[i]);
      if (!Item::is_breakable (it) && !Paper_column::is_musical (it))
	continue;

      // it->breakable || it->musical

      /*
	sloppy with typing left/right-neighbors should take list, but paper-column found instead.
      */
      extract_grob_set (cols[i], "left-neighbors", lns);
      if (lns.is_empty () && i )
	{
	  SCM ga_scm = Grob_array::make_array();
	  Grob_array *ga = unsmob_grob_array (ga_scm);
	  ga->add (cols[i-1]);
	  cols[i]->set_object ("left-neighbors", ga_scm);
	}
      extract_grob_set (cols[i], "right-neighbors", rns);
      if (rns.is_empty () && i < cols.size () - 1)
	{
	  SCM ga_scm = Grob_array::make_array();
	  Grob_array *ga = unsmob_grob_array (ga_scm);
	  ga->add (cols[i+1]);
	  cols[i]->set_object ("right-neighbors", ga_scm);
	}
    }
}
