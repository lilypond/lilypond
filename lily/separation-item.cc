/*
  separation-item.cc -- implement Separation_item

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "separation-item.hh"

#include "skyline.hh"
#include "paper-column.hh"
#include "warn.hh"
#include "pointer-group-interface.hh"
#include "accidental-placement.hh"

void
Separation_item::add_item (Grob *s, Item *i)
{
  assert (i);
  Pointer_group_interface::add_grob (s, ly_symbol2scm ("elements"), i);
}

void
Separation_item::add_conditional_item (Grob *me, Grob *e)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("conditional-elements"), e);
}

void
Separation_item::set_skyline_distance (Drul_array<Item *> items,
				       Real padding)
{
  Drul_array<Skyline*> lines;
  Direction d = LEFT;

  do
    {
      SCM prop = items[d]->get_property ("skylines");
      lines[d] = Skyline::unsmob (index_get_cell (prop, -d));
    }
  while (flip (&d) != LEFT);

  Real dist = padding + lines[LEFT]->distance (*lines[RIGHT]);
  if (dist > 0)
    {
      Rod rod;

      rod.item_drul_ = items;

      rod.distance_ = dist;
      rod.add_to_cols ();
    }  
}

bool
Separation_item::set_distance (Drul_array<Item *> items,
			       Real padding)
{
  if (!Item::is_non_musical (items[LEFT])
      && !Item::is_non_musical (items[RIGHT]))
    {
      set_skyline_distance (items, padding);
      return true;
    }
  
  Interval li (Separation_item::width (items[LEFT]));
  Interval ri (Separation_item::conditional_width (items[RIGHT], items[LEFT]));
  if (!li.is_empty () && !ri.is_empty ())
    {
      Rod rod;

      rod.item_drul_ = items;

      rod.distance_ = li[RIGHT] - ri[LEFT] + padding;

      if (rod.distance_  > 0)
	rod.add_to_cols ();
      return true;
    }
  return false;
}

/*
  Return the width of ME given that we are considering the object on
  the LEFT.
*/
Interval
Separation_item::conditional_width (Grob *me, Grob *left)
{
  Interval w = width (me);

  Item *item = dynamic_cast<Item *> (me);
  Paper_column *pc = item->get_column ();

  extract_grob_set (me, "conditional-elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    {
      Item *il = dynamic_cast<Item *> (elts[i]);
      if (pc != il->get_column ())
	{
	  programming_error ("Separation_item element from wrong column");
	  continue;
	}

      if (to_boolean (il->get_property ("no-spacing-rods")))
	continue;

      if (Accidental_placement::has_interface (il))
	w.unite (Accidental_placement::get_relevant_accidental_extent (il, pc, left));
    }

  SCM pad = me->get_property ("padding");

  w.widen (robust_scm2double (pad, 0.0));
  return w;
}


MAKE_SCHEME_CALLBACK(Separation_item,calc_skylines,1);
SCM
Separation_item::calc_skylines (SCM smob)
{
  Item *me = unsmob_item (smob);
  SCM lines = scm_cons (SCM_BOOL_F,SCM_BOOL_F);

  Direction d = LEFT;
  vector<Box> bs = boxes (me);
  do
    {
      Skyline l (bs, 0, Y_AXIS, d);
      index_set_cell (lines, d, l.smobbed_copy ());
    }
  while (flip (&d) != LEFT);

  return lines;
}


vector<Box>
Separation_item::boxes (Grob *me)
{
  Item *item = dynamic_cast<Item *> (me);

  int very_large = INT_MAX;
  Paper_column *pc = item->get_column ();
  vector<Box> out;
  extract_grob_set (me, "elements", elts);

  Grob *ycommon = common_refpoint_of_array (elts, me, Y_AXIS);
  
  for (vsize i = 0; i < elts.size (); i++)
    {
      Item *il = dynamic_cast<Item *> (elts[i]);
      if (pc != il->get_column ())
	{
	  continue;
	}

      if (to_boolean (il->get_property ("no-spacing-rods")))
	continue;

      Interval y (il->pure_height (ycommon, 0, very_large));
      y.widen (0.1);		// fixme
      Box b (il->extent (pc, X_AXIS), y);

      out.push_back (b);
    }

  return out;      
}

Interval
Separation_item::width (Grob *me)
{
  SCM sw = me->get_property ("X-extent");
  if (is_number_pair (sw))
    return ly_scm2interval (sw);

  Item *item = dynamic_cast<Item *> (me);
  Paper_column *pc = item->get_column ();
  Interval w;

  extract_grob_set (me, "elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    {
      Item *il = dynamic_cast<Item *> (elts[i]);
      if (pc != il->get_column ())
	{
	  /* this shouldn't happen, but let's continue anyway. */
	  programming_error ("Separation_item:  I've been drinking too much");
	  continue;		/*UGH UGH*/
	}

      if (to_boolean (il->get_property ("no-spacing-rods")))
	continue;

      Interval iv (il->extent (pc, X_AXIS));
      if (!iv.is_empty ())
	w.unite (iv);
    }

  SCM pad = me->get_property ("padding");

  w.widen (robust_scm2double (pad, 0.0));

  me->set_property ("X-extent", ly_interval2scm (w));

  return w;
}

Interval
Separation_item::relative_width (Grob *me, Grob *common)
{
  Interval iv = width (me);

  return dynamic_cast<Item *> (me)->get_column ()->relative_coordinate (common, X_AXIS) + iv;
}

/*
  Try to find the break-aligned symbol in SEPARATION_ITEM that is
  sticking out at direction D. The x size is put in LAST_EXT
*/
Grob *
Separation_item::extremal_break_aligned_grob (Grob *me,
					      Direction d,
					      Interval *last_ext)
{
  Grob *col = dynamic_cast<Item *> (me)->get_column ();
  last_ext->set_empty ();
  Grob *last_grob = 0;

  extract_grob_set (me, "elements", elts);
  for (vsize i = elts.size (); i--;)
    {
      Grob *break_item = elts[i];
      if (!scm_is_symbol (break_item->get_property ("break-align-symbol")))
	continue;

      if (!scm_is_pair (break_item->get_property ("space-alist")))
	continue;

      Interval ext = break_item->extent (col, X_AXIS);

      if (ext.is_empty ())
	continue;

      if (!last_grob
	  || (last_grob && d * (ext[d]- (*last_ext)[d]) > 0))
	{
	  *last_ext = ext;
	  last_grob = break_item;
	}
    }

  return last_grob;
}

ADD_INTERFACE (Separation_item,
	       "Item that computes widths to generate spacing rods. "
	       "This is done in concert with @ref{separating-group-spanner-interface}.",

	       "X-extent "
	       "conditional-elements "
	       "elements "
	       "padding "
	       "skylines "
	       );
