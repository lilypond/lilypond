/*
  spacing-interface.cc -- functionality that is shared between Note_spacing
  and Staff_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2007 Joe Neeman <joeneeman@gmail.com>
*/

#include "spacing-interface.hh"

#include "grob.hh"
#include "grob-array.hh"
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "paper-column.hh"
#include "separation-item.hh"
#include "skyline.hh"
#include "skyline-pair.hh"
#include "system.hh"

/* return the right-pointing skyline of the left-items and the left-pointing
   skyline of the right-items (with the skyline of the left-items in
   ret[LEFT]) */
Drul_array<Skyline>
Spacing_interface::skylines (Grob *me, Grob *right_col)
{
  /* the logic here is a little convoluted.
     A {Staff,Note}_spacing doesn't copy left-items when it clones,
     so in order to find the separation items, we need to use the original
     spacing grob. But once we find the separation items, we need to get back
     the broken piece.
  */

  Grob *orig = me->original () ? me->original () : me;
  Drul_array<Direction> break_dirs (dynamic_cast<Item*> (me)->break_status_dir (),
				    dynamic_cast<Item*> (right_col)->break_status_dir ());
  Drul_array<Skyline> skylines = Drul_array<Skyline> (Skyline (RIGHT), Skyline (LEFT));
  Drul_array<vector<Grob*> > items (ly_scm2link_array (orig->get_object ("left-items")),
				    ly_scm2link_array (orig->get_object ("right-items")));

  Grob *system = me->get_system ();
  Grob *left_col = dynamic_cast<Item*> (me)->get_column ();

  Drul_array<Grob*> columns (left_col, right_col);

  Direction d = LEFT;
  do
    {
      for (vsize i = 0; i < items[d].size (); i++)
	{
	  Item *g = dynamic_cast<Item*> (items[d][i]);
	  if (g)
	    if (Item *piece = g->find_prebroken_piece (break_dirs[d]))
	      g = piece;

	  if (g && Separation_item::has_interface (g) && g->get_column () == columns[d])
	    {
	      SCM sky_scm = g->get_property ("horizontal-skylines");
	      Skyline_pair *sky = Skyline_pair::unsmob (sky_scm);

	      extract_grob_set (g, "elements", elts);
	      Grob *ycommon = common_refpoint_of_array (elts, g, Y_AXIS);
	      Real shift = ycommon->pure_relative_y_coordinate (system, 0, INT_MAX);

	      skylines[d].shift (-shift);

	      if (sky)
		skylines[d].merge ((*sky)[-d]);
	      else
		programming_error ("separation item has no skyline");

	      if (d == RIGHT && items[LEFT].size ())
		skylines[d].merge (Separation_item::conditional_skyline (items[d][i], items[LEFT][0]));

	      skylines[d].shift (shift);
	    }
	}
    }
  while (flip (&d) != LEFT);

  return skylines;
}

Real
Spacing_interface::minimum_distance (Grob *me, Grob *right)
{
  Drul_array<Skyline> skylines = Spacing_interface::skylines (me, right);

  return max (0.0, skylines[LEFT].distance (skylines[RIGHT]));
}

/*
  Compute the column of the right-items.  This is a big function,
  since RIGHT-ITEMS may span more columns (eg. if a clef is inserted,
  this will add a new column to RIGHT-ITEMS. Here we look at the
  columns, and return the left-most. If there are multiple columns, we
  prune RIGHT-ITEMS.

  If we end up pruning, we add a left-neighbor to every column that
  gets pruned. This ensures that loose columns in cross-staff music
  do indeed get marked as loose. The problem situation is when a voice
  passes from staff 1 to staff 2 and a clef appears later on in staff 1.
  Then the NoteSpacing attached to the last note in staff 1 has two
  right-items: one pointing to the next note in staff 2 and one pointing
  to the clef. We will prune the clef right-item here and, unless we add
  a left-neighbor to the clef, it won't get marked as loose.
*/
Item *
Spacing_interface::right_column (Grob *me)
{
  if (!me->is_live ())
    return 0;

  Grob_array *a = unsmob_grob_array (me->get_object ("right-items"));
  Item *mincol = 0;
  int min_rank = INT_MAX;
  bool prune = false;
  for (vsize i = 0; a && i < a->size (); i++)
    {
      Item *ri = a->item (i);
      Item *col = ri->get_column ();

      int rank = Paper_column::get_rank (col);

      if (rank < min_rank)
	{
	  min_rank = rank;
	  if (mincol)
	    prune = true;

	  mincol = col;
	}
      else if (rank > min_rank)
	prune = true;
    }

  if (prune && a)
    {
      vector<Grob*> &right = a->array_reference ();
      for (vsize i = right.size (); i--;)
	{
	  if (dynamic_cast<Item *> (right[i])->get_column () != mincol)
	    {
	      extract_grob_set (right[i], "left-neighbors", lns);
	      if (lns.empty ())
		Pointer_group_interface::add_grob (right[i],
						   ly_symbol2scm ("left-neighbors"),
						   dynamic_cast<Item*> (me)->get_column ());

	      right.erase (right.begin () + i);
	    }
	}
    }

  return mincol;
}

Item *
Spacing_interface::left_column (Grob *me)
{
  if (!me->is_live ())
    return 0;

  return dynamic_cast<Item *> (me)->get_column ();
}

static vector<Item*>
get_note_columns (vector<Grob*> const &elts)
{
  vector<Item*> ret;

  for (vsize i = 0; i < elts.size (); i++)
    {
      if (Note_column::has_interface (elts[i]))
	ret.push_back (dynamic_cast<Item*> (elts[i]));
      else if (Separation_item::has_interface (elts[i]))
	{
	  extract_grob_set (elts[i], "elements", more_elts);
	  vector<Item*> ncs = get_note_columns (more_elts);

	  ret.insert (ret.end (), ncs.begin (), ncs.end ());
	}
    }

  return ret;
}

vector<Item*>
Spacing_interface::right_note_columns (Grob *me)
{
  extract_grob_set (me, "right-items", elts);
  return get_note_columns (elts);
}

vector<Item*>
Spacing_interface::left_note_columns (Grob *me)
{
  extract_grob_set (me, "left-items", elts);
  return get_note_columns (elts);
}

/*
  Try to find the break-aligned symbol that belongs on the D-side
  of ME, sticking out in direction -D. The x size is put in LAST_EXT
*/
Grob *
Spacing_interface::extremal_break_aligned_grob (Grob *me,
						Direction d,
						Direction break_dir,
						Interval *last_ext)
{
  Grob *col = 0;
  last_ext->set_empty ();
  Grob *last_grob = 0;

  extract_grob_set (me, d == LEFT ? "left-break-aligned" : "right-break-aligned", elts);

  for (vsize i = elts.size (); i--;)
    {
      Item *break_item = dynamic_cast<Item*> (elts[i]);

      if (break_item->break_status_dir () != break_dir)
	break_item = break_item->find_prebroken_piece (break_dir);

      if (!break_item || !scm_is_pair (break_item->get_property ("space-alist")))
	continue;

      if (!col)
	col = dynamic_cast<Item*> (elts[0])->get_column ()->find_prebroken_piece (break_dir);

      Interval ext = break_item->extent (col, X_AXIS);

      if (ext.is_empty ())
	continue;

      if (!last_grob
	  || (last_grob && d * (ext[-d]- (*last_ext)[-d]) < 0))
	{
	  *last_ext = ext;
	  last_grob = break_item;
	}
    }

  return last_grob;
}


ADD_INTERFACE (Spacing_interface,
	       "This object calculates the desired and minimum distances"
	       " between two columns.",

	       /* properties */
	       "left-items "
	       "right-items "
	       );
