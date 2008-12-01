/*
  spacing-determine-loose-columns.cc -- implement Spacing_spanner
  methods that decide which columns to turn loose.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

  extract_grob_set (col, "right-neighbors", rns);
  extract_grob_set (col, "left-neighbors", lns);

  if (lns.empty () || rns.empty ())
    return false;

  
  Item *l_neighbor = dynamic_cast<Item *> (lns[0]);
  Item *r_neighbor = dynamic_cast<Item *> (rns[0]);

  if (!l_neighbor || !r_neighbor)
    return false;

  l_neighbor = l_neighbor->get_column ();
  r_neighbor = dynamic_cast<Item *> (Spacing_interface::right_column (r_neighbor));

  if (l == l_neighbor && r == r_neighbor)
    return false;

  if (!l_neighbor || !r_neighbor)
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
	  extract_grob_set (c, "right-neighbors", rns_arr);
	  extract_grob_set (c, "left-neighbors", lns_arr);

	  SCM lns = lns_arr.size () ? lns_arr.back ()->self_scm () : SCM_BOOL_F;
	  SCM rns = rns_arr.size () ? rns_arr.back ()->self_scm () : SCM_BOOL_F;

	  /*
	    Either object can be non existent, if the score ends
	    prematurely.
	  */

	  extract_grob_set (unsmob_grob (rns), "right-items", right_items);
	  if (right_items.size () == 0 || !unsmob_grob (lns))
	    {
	      c->programming_error ("Cannot determine neighbors for floating column. ");
	      c->set_object ("between-cols", scm_cons (cols->at (i-1)->self_scm (),
						       cols->at (i+1)->self_scm ()));
	    }
	  else
	    {
	      Grob *min_item = 0;
	      int min_rank = INT_MAX;
	      for (vsize j = 0; j < right_items.size (); j ++)
		{
		  int rank = dynamic_cast<Item*> (right_items[j])->get_column ()->get_rank ();
		  if (rank < min_rank)
		    {
		      min_item = right_items[j];
		      min_rank = rank;
		    }
		}
	      
	      c->set_object ("between-cols", scm_cons (lns,
						       min_item->self_scm ()));

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
      SCM right_neighbors = Grob_array::make_array ();
      Grob_array *rn_arr = unsmob_grob_array (right_neighbors);
      int min_rank = INT_MAX;

      extract_grob_set (cols[i], "spacing-wishes", wishes);
      for (vsize k = wishes.size (); k--;)
	{
	  Item *wish = dynamic_cast<Item *> (wishes[k]);

	  Item *lc = wish->get_column ();
	  Grob *right = Spacing_interface::right_column (wish);

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
	      Item *it = dynamic_cast<Item *> (lns_arr.back ());
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
	cols[i]->set_object ("right-neighbors", right_neighbors);
    }
}

/*
  Set neighboring columns that have no left/right-neighbor set
  yet. Only do breakable non-musical columns, and musical columns.
*/
void
Spacing_spanner::set_implicit_neighbor_columns (vector<Grob*> const &cols)
{
  for (vsize i = 0; i < cols.size (); i++)
    {
      Item *it = dynamic_cast<Item *> (cols[i]);
      if (!Paper_column::is_breakable (it) && !Paper_column::is_musical (it))
	continue;

      /*
	sloppy with typing left/right-neighbors should take list, but paper-column found instead.
      */
      extract_grob_set (cols[i], "left-neighbors", lns);
      if (lns.empty () && i)
	{
	  SCM ga_scm = Grob_array::make_array ();
	  Grob_array *ga = unsmob_grob_array (ga_scm);
	  ga->add (cols[i - 1]);
	  cols[i]->set_object ("left-neighbors", ga_scm);
	}
      extract_grob_set (cols[i], "right-neighbors", rns);
      if (rns.empty () && i + 1 < cols.size ())
	{
	  SCM ga_scm = Grob_array::make_array ();
	  Grob_array *ga = unsmob_grob_array (ga_scm);
	  ga->add (cols[i + 1]);
	  cols[i]->set_object ("right-neighbors", ga_scm);
	}
    }
}
