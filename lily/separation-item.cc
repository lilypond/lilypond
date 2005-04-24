/*
  separation-item.cc -- implement Separation_item

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "separation-item.hh"

#include "paper-column.hh"
#include "warn.hh"
#include "group-interface.hh"
#include "accidental-placement.hh"

void
Separation_item::add_item (Grob *s, Item *i)
{
  assert (i);
  Pointer_group_interface::add_grob (s, ly_symbol2scm ("elements"), i);
  s->add_dependency (i);
}

void
Separation_item::add_conditional_item (Grob *me, Grob *e)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("conditional-elements"), e);
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

  for (SCM s = me->get_property ("conditional-elements"); scm_is_pair (s); s = scm_cdr (s))
    {
      SCM elt = scm_car (s);
      if (!unsmob_grob (elt))
	continue;

      Item *il = unsmob_item (elt);
      if (pc != il->get_column ())
	{
	  /* this shouldn't happen, but let's continue anyway. */
	  programming_error ("Separation_item:  I've been drinking too much");
	  continue;		/*UGH UGH*/
	}

      if (to_boolean (il->get_property ("no-spacing-rods")))
	{
	  continue;
	}

      if (Accidental_placement::has_interface (il))
	{
	  w.unite (Accidental_placement::get_relevant_accidental_extent (il, pc, left));
	}
    }

  SCM pad = me->get_property ("padding");

  w.widen (robust_scm2double (pad, 0.0));
  return w;
}

Interval
Separation_item::width (Grob *me)
{
  SCM sw = me->get_property ("X-extent");
  if (is_number_pair (sw))
    {
      return ly_scm2interval (sw);
    }

  Item *item = dynamic_cast<Item *> (me);
  Paper_column *pc = item->get_column ();
  Interval w;

  for (SCM s = me->get_property ("elements"); scm_is_pair (s); s = scm_cdr (s))
    {
      SCM elt = scm_car (s);
      if (!unsmob_grob (elt))
	continue;

      Item *il = unsmob_item (elt);
      if (pc != il->get_column ())
	{
	  /* this shouldn't happen, but let's continue anyway. */
	  programming_error ("Separation_item:  I've been drinking too much");
	  continue;		/*UGH UGH*/
	}

      if (to_boolean (il->get_property ("no-spacing-rods")))
	{
	  continue;
	}

      Interval iv (il->extent (pc, X_AXIS));
      if (!iv.is_empty ())
	{
	  w.unite (iv);
	}
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
Separation_item::extremal_break_aligned_grob (Grob *separation_item, Direction d,
					      Interval *last_ext)
{
  Grob *col = dynamic_cast<Item *> (separation_item)->get_column ();
  last_ext->set_empty ();
  Grob *last_grob = 0;
  for (SCM s = separation_item->get_property ("elements");
       scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *break_item = unsmob_grob (scm_car (s));

      if (!scm_is_symbol (break_item->get_property ("break-align-symbol")))
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

ADD_INTERFACE (Separation_item, "separation-item-interface",
	       "Item that computes widths to generate spacing rods. "
	       "This is done in concert with @ref{separation-spanner-interface}.",
	       "padding X-extent conditional-elements elements");
