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

/* return the minimum distance between the left-items and the right-items of
   this spacing object */
Real
Spacing_interface::minimum_distance (Grob *me)
{
  /* the logic here is a little convoluted.
     A {Staff,Note}_spacing doesn't copy {left-,right-}items when it clones,
     so in order to find the separation items, we need to use the original
     spacing grob. But once we find the separation items, we need to get back
     the broken piece.

     FIXME: this only works for the left column. There is only one spacing
     grob for the original and non-original right column and we have no way
     to tell which one we need */

  Grob *orig = me->original () ? me->original () : me;
  Direction break_dir = dynamic_cast<Item*> (me)->break_status_dir ();
  Drul_array<Skyline> skylines = Drul_array<Skyline> (Skyline (RIGHT), Skyline (LEFT));
  Drul_array<vector<Grob*> > items (ly_scm2link_array (orig->get_object ("left-items")),
				    ly_scm2link_array (orig->get_object ("right-items")));

  Direction d = LEFT;
  do
    {
      for (vsize i = 0; i < items[d].size (); i++)
	{
	  Grob *g = items[d][i];
	  if (d == LEFT)
	    if (Item *it = dynamic_cast<Item*> (g))
	      if (Grob *piece = it->find_prebroken_piece (break_dir))
		g = piece;

	  if (Separation_item::has_interface (g))
	    {
	      SCM sky_scm = g->get_property ("horizontal-skylines");
	      Skyline_pair *sky = Skyline_pair::unsmob (sky_scm);
	      if (sky)
		skylines[d].merge ((*sky)[-d]);
	      else
		programming_error ("separation item has no skyline");
	    
	      if (d == RIGHT && items[LEFT].size ())
		skylines[d].merge (Separation_item::conditional_skyline (items[d][i], items[LEFT][0]));
	    }
	}
    }
  while (flip (&d) != LEFT);

  return max (0.0, skylines[LEFT].distance (skylines[RIGHT]));
}

/*
  Compute the column of the right-items.  This is a big function,
  since RIGHT-ITEMS may span more columns (eg. if a clef is inserted,
  this will add a new column to RIGHT-ITEMS. Here we look at the
  columns, and return the left-most. If there are multiple columns, we
  prune RIGHT-ITEMS.
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
    }

  if (prune && a)
    {
      vector<Grob*> &right = a->array_reference ();
      for (vsize i = right.size (); i--;)
	{
	  if (dynamic_cast<Item *> (right[i])->get_column () != mincol)
	    right.erase (right.begin () + i);
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
    if (Note_column::has_interface (elts[i]))
      ret.push_back (dynamic_cast<Item*> (elts[i]));

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

ADD_INTERFACE (Spacing_interface,
	       "This object calculates the desired and minimum distances between two columns.",

	       "left-items "
	       "right-items "
	       );
