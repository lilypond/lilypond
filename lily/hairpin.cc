/*
  hairpin.cc -- implement Hairpin

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "hairpin.hh"

#include "dimensions.hh"
#include "font-interface.hh"
#include "international.hh"
#include "line-interface.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "text-interface.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Hairpin, after_line_breaking, 1);
SCM
Hairpin::after_line_breaking (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));
  consider_suicide (me);

  return SCM_UNSPECIFIED;
}

void
Hairpin::consider_suicide (Spanner*me)
{
  Drul_array<bool> broken;
  Drul_array<Item *> bounds;
  Direction d = LEFT;
  do
    {
      bounds[d] = me->get_bound (d);
      broken[d] = bounds[d]->break_status_dir () != CENTER;
    }
  while (flip (&d) != LEFT);

  if (broken[LEFT]
      && ly_is_equal (bounds[RIGHT]->get_column ()->get_property ("when"),
		      bounds[LEFT]->get_property ("when")))
    me->suicide ();
  
}

MAKE_SCHEME_CALLBACK (Hairpin, print, 1);

SCM
Hairpin::print (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));

  consider_suicide (me);
  SCM s = me->get_property ("grow-direction");
  if (!is_direction (s))
    {
      me->suicide ();
      return SCM_EOL;
    }

  Direction grow_dir = to_dir (s);
  Real padding = robust_scm2double (me->get_property ("bound-padding"), 0.5);

  Drul_array<bool> broken;
  Drul_array<Item *> bounds;
  Direction d = LEFT;
  do
    {
      bounds[d] = me->get_bound (d);
      broken[d] = bounds[d]->break_status_dir () != CENTER;
    }
  while (flip (&d) != LEFT);
  if (broken[RIGHT])
    {
      Spanner *orig = dynamic_cast<Spanner*> (me->original ());
      if (me->get_break_index ()
	  < orig->broken_intos_.size () - 1)
	{
	  Spanner *next = orig->broken_intos_[me->get_break_index () + 1];
	  Stencil *s = next->get_stencil ();
	  if (!s || s->is_empty ())
	    broken[RIGHT] = false;
	}
    }

  Grob *common = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);
  Drul_array<Real> x_points;

  do
    {
      Item *b = bounds[d];
      x_points[d] = b->relative_coordinate (common, X_AXIS);
      if (broken [d])
	{
	  if (d == LEFT)
	    x_points[d] = b->extent (common, X_AXIS)[RIGHT];
	}
      else
	{
	  if (Text_interface::has_interface (b))
	    {
	      Interval e = b->extent (common, X_AXIS);
	      if (!e.is_empty ())
		x_points[d] = e[-d] - d * padding;
	    }
	  else
	    {
	      bool neighbor_found = false;
	      extract_grob_set (me, "adjacent-hairpins", pins);
	      for (vsize i = 0; i < pins.size (); i++)
		{
		  /*
		    FIXME: this will fuck up in case of polyphonic
		    notes in other voices. Need to look at note-columns
		    in the current staff/voice.
		  */

		  Spanner *pin = dynamic_cast<Spanner *> (pins[i]);
		  if (pin
		      && (pin->get_bound (LEFT)->get_column () == b->get_column ()
			  || pin->get_bound (RIGHT)->get_column () == b->get_column ()))
		    neighbor_found = true;
		}

	      /*
		If we're hung on a paper column, that means we're not
		adjacent to a text-dynamic, and we may move closer. We
		make the padding a little smaller, here.
	      */
	      Interval e = robust_relative_extent (b, common, X_AXIS);
	      x_points[d]
		= neighbor_found ? e.center () - d * padding / 3 : e[d];
	    }
	}
    }
  while (flip (&d) != LEFT);

  Real width = x_points[RIGHT] - x_points[LEFT];
  if (width < 0)
    {
      me->warning (_ ((grow_dir < 0) ? "decrescendo too small"
		      : "crescendo too small"));
      width = 0;
    }

  bool continued = broken[Direction (-grow_dir)];
  Real height = robust_scm2double (me->get_property ("height"), 0.2) *
    Staff_symbol_referencer::staff_space (me);

  Real starth, endh;
  if (grow_dir < 0)
    {
      starth = height;
      endh = continued ? height / 2 : 0.0;
    }
  else
    {
      starth = continued ? height / 2 : 0.0;
      endh = height;
    }

  /*
    should do relative to staff-symbol staff-space?
  */

  Stencil mol;
  mol = Line_interface::line (me, Offset (0, starth), Offset (width, endh));
  mol.add_stencil (Line_interface::line (me,
					 Offset (0, -starth),
					 Offset (width, -endh)));

  mol.translate_axis (x_points[LEFT]
		      - bounds[LEFT]->relative_coordinate (common, X_AXIS),
		      X_AXIS);
  return mol.smobbed_copy ();
}

ADD_INTERFACE (Hairpin, "hairpin-interface",
	       "A hairpin crescendo/decrescendo.",
	       "adjacent-hairpins "
	       "bound-padding "
	       "grow-direction "
	       "height "
	       );

