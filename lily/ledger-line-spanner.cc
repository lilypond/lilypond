/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <map>
using namespace std;

#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "pointer-group-interface.hh"
#include "paper-column.hh"

struct Ledger_line_spanner
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  static Stencil brew_ledger_lines (Grob *me,
				    int pos,
				    Interval,
				    Real, Real,
				    Interval x_extent,
				    Real left_shorten);

  DECLARE_GROB_INTERFACE ();
};

Stencil
Ledger_line_spanner::brew_ledger_lines (Grob *staff,
					int pos,
					Interval staff_extent,
					Real halfspace,
					Real ledgerlinethickness,
					Interval x_extent,
					Real left_shorten)
{
  int line_count = (staff_extent.contains (pos)
		    ? 0
		    : sign (pos) * int (rint (pos -  staff_extent[Direction (sign (pos))])) / 2);
  Stencil stencil;
  if (line_count)
    {
      Real blotdiameter = ledgerlinethickness;
      Interval y_extent
	= Interval (-0.5 * (ledgerlinethickness),
		    +0.5 * (ledgerlinethickness));
      Stencil proto_ledger_line
	= Lookup::round_filled_box (Box (x_extent, y_extent), blotdiameter);

      x_extent[LEFT] += left_shorten;
      Stencil proto_first_line
	= Lookup::round_filled_box (Box (x_extent, y_extent), blotdiameter);

      Direction dir = (Direction)sign (pos);
      Real offs = (Staff_symbol_referencer::on_line (staff, pos))
	? 0.0
	: -dir * halfspace;

      offs += pos * halfspace;
      for (int i = 0; i < line_count; i++)
	{
	  Stencil ledger_line ((i == 0)
			       ? proto_first_line
			       : proto_ledger_line);
	  ledger_line.translate_axis (-dir * halfspace * i * 2 + offs, Y_AXIS);
	  stencil.add_stencil (ledger_line);
	}
    }

  return stencil;
}

static void
set_rods (Drul_array<Interval> const &current_extents,
	  Drul_array<Interval> const &previous_extents,
	  Item *current_column,
	  Item *previous_column,
	  Real min_length_fraction)
{
  Direction d = UP;
  do
    {
      if (!current_extents[d].is_empty ()
	  && !previous_extents[d].is_empty ())
	{
	  Real total_head_length = previous_extents[d].length ()
	    + current_extents[d].length ();

	  Rod rod;
	  rod.distance_ = total_head_length
	    * (3 / 2 * min_length_fraction)
	    /*
	      we go from right to left.
	    */
	    - previous_extents[d][LEFT]
	    + current_extents[d][RIGHT];

	  rod.item_drul_[LEFT] = current_column;
	  rod.item_drul_[RIGHT] = previous_column;
	  rod.add_to_cols ();
	}
    }
  while (flip (&d) != DOWN);
}

MAKE_SCHEME_CALLBACK (Ledger_line_spanner, set_spacing_rods, 1);
SCM
Ledger_line_spanner::set_spacing_rods (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));

  // find size of note heads.
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (!staff)
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real min_length_fraction
    = robust_scm2double (me->get_property ("minimum-length-fraction"), 0.15);

  Drul_array<Interval> current_extents;
  Drul_array<Interval> previous_extents;
  Item *previous_column = 0;
  Item *current_column = 0;

  Real halfspace = Staff_symbol::staff_space (staff) / 2;

  Interval staff_extent = staff->extent (staff, Y_AXIS);
  staff_extent *= 1 / halfspace;
    
  /*
    Run through heads using a loop. Since Ledger_line_spanner can
    contain a lot of noteheads, superlinear performance is too slow.
  */
  extract_item_set (me, "note-heads", heads);
  for (vsize i = heads.size (); i--;)
    {
      Item *h = heads[i];

      int pos = Staff_symbol_referencer::get_rounded_position (h);
      if (staff_extent.contains (pos))
	continue;

      Item *column = h->get_column ();
      if (current_column != column)
	{
	  set_rods (current_extents, previous_extents,
		    current_column, previous_column,
		    min_length_fraction);

	  previous_column = current_column;
	  current_column = column;
	  previous_extents = current_extents;

	  current_extents[DOWN].set_empty ();
	  current_extents[UP].set_empty ();
	}

      Interval head_extent = h->extent (column, X_AXIS);
      Direction vdir = Direction (sign (pos));
      if (!vdir)
	continue;

      current_extents[vdir].unite (head_extent);
    }

  if (previous_column && current_column)
    set_rods (current_extents, previous_extents,
	      current_column, previous_column,
	      min_length_fraction);

  return SCM_UNSPECIFIED;
}

struct Ledger_request
{
  Interval ledger_extent_;
  Interval head_extent_;
  int position_;
  bool excentric_;
  Ledger_request ()
  {
    ledger_extent_.set_empty ();
    head_extent_.set_empty ();
    position_ = 0;
  }
};

typedef map < int, Drul_array<Ledger_request> > Ledger_requests;

/*
  TODO: ledger share a lot of info. Lots of room to optimize away
  common use of objects/variables.
*/
MAKE_SCHEME_CALLBACK (Ledger_line_spanner, print, 1);
SCM
Ledger_line_spanner::print (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));

  extract_grob_set (me, "note-heads", heads);

  if (heads.empty ())
    return SCM_EOL;

  // find size of note heads.
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (!staff)
    return SCM_EOL;

  Real halfspace = Staff_symbol::staff_space (staff) / 2;

  Interval staff_extent = staff->extent (staff, Y_AXIS);
  staff_extent *= 1 / halfspace;
  
  Real length_fraction
    = robust_scm2double (me->get_property ("length-fraction"), 0.25);

  Stencil ledgers;
  Stencil default_ledger;

  Grob *common[NO_AXES];

  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis a = Axis (i);
      common[a] = common_refpoint_of_array (heads, me, a);
      for (vsize i = heads.size (); i--;)
	if (Grob *g = unsmob_grob (me->get_object ("accidental-grob")))
	  common[a] = common[a]->common_refpoint (g, a);
    }

  Ledger_requests reqs;
  for (vsize i = heads.size (); i--;)
    {
      Item *h = dynamic_cast<Item *> (heads[i]);

      int pos = Staff_symbol_referencer::get_rounded_position (h);
      if (pos && !staff_extent.contains (pos))
	{
	  Interval head_extent = h->extent (common[X_AXIS], X_AXIS);
	  Interval ledger_extent = head_extent;
	  ledger_extent.widen (length_fraction * head_extent.length ());

	  Direction vdir = Direction (sign (pos));
	  int rank = h->get_column ()->get_rank ();

	  reqs[rank][vdir].ledger_extent_.unite (ledger_extent);
	  reqs[rank][vdir].head_extent_.unite (head_extent);
	  reqs[rank][vdir].position_
	    = vdir * max (vdir * reqs[rank][vdir].position_, vdir * pos);
	}
    }

  // determine maximum size for non-colliding ledger.
  Real gap = robust_scm2double (me->get_property ("gap"), 0.1);
  Ledger_requests::iterator last (reqs.end ());
  for (Ledger_requests::iterator i (reqs.begin ());
       i != reqs.end (); last = i++)
    {
      if (last == reqs.end ())
	continue;

      Direction d = DOWN;
      do
	{
	  if (!staff_extent.contains (last->second[d].position_)
	      && !staff_extent.contains (i->second[d].position_))
	    {
	      Real center
		= (last->second[d].head_extent_[RIGHT]
		   + i->second[d].head_extent_[LEFT]) / 2;

	      Direction which = LEFT;
	      do
		{
		  Ledger_request &lr = ((which == LEFT) ? * last : *i).second[d];

		  // due tilt of quarter note-heads
		  /* FIXME */
		  bool both
		    = (!staff_extent.contains (last->second[d].position_
					       - sign (last->second[d].position_))
		       && !staff_extent.contains (i->second[d].position_
						  - sign (i->second[d].position_)));
		  Real limit = (center + (both ? which * gap / 2 : 0));
		  lr.ledger_extent_.at (-which)
		    = which * max (which * lr.ledger_extent_[-which], which * limit);
		}
	      while (flip (&which) != LEFT);
	    }
	}
      while (flip (&d) != DOWN);
    }

  // create ledgers for note heads
  Real ledgerlinethickness
    = Staff_symbol::get_ledger_line_thickness (staff);
  for (vsize i = heads.size (); i--;)
    {
      Item *h = dynamic_cast<Item *> (heads[i]);

      int pos = Staff_symbol_referencer::get_rounded_position (h);
      if (!staff_extent.contains (pos - sign (pos)))
	{
	  Interval head_size = h->extent (common[X_AXIS], X_AXIS);
	  Interval ledger_size = head_size;
	  ledger_size.widen (ledger_size.length () * length_fraction);

	  Interval max_size = reqs[h->get_column ()->get_rank ()]
	    [Direction (sign (pos))].ledger_extent_;

	  ledger_size.intersect (max_size);
	  Real left_shorten = 0.0;
	  if (Grob *g = unsmob_grob (h->get_object ("accidental-grob")))
	    {
	      Interval accidental_size = g->extent (common[X_AXIS], X_AXIS);
	      Real d
		= linear_combination (Drul_array<Real> (accidental_size[RIGHT],
							head_size[LEFT]),
				      0.0);

	      left_shorten = max (-ledger_size[LEFT] + d, 0.0);

	      /*
		TODO: shorten 2 ledger lines for the case natural +
		downstem.
	      */
	    }

	  ledgers.add_stencil (brew_ledger_lines (staff, pos, staff_extent,
						  halfspace,
						  ledgerlinethickness,
						  ledger_size,
						  left_shorten));
	}
    }

  ledgers.translate_axis (-me->relative_coordinate (common[X_AXIS], X_AXIS),
			  X_AXIS);

  return ledgers.smobbed_copy ();
}

ADD_INTERFACE (Ledger_line_spanner,
	       "This spanner draws the ledger lines of a staff.  This is a"
	       " separate grob because it has to process all potential"
	       " collisions between all note heads.",

	       /* properties */
	       "gap "	
	       "length-fraction "	
	       "minimum-length-fraction "
	       "note-heads "
	       "thickness "
	       );

struct Ledgered_interface
{
  DECLARE_GROB_INTERFACE ();
};

ADD_INTERFACE (Ledgered_interface,
	       "Objects that need ledger lines, typically note heads.  See"
	       " also @ref{ledger-line-spanner-interface}.",

	       /* properties */
	       "no-ledgers "
	       );
