/*
  ledger-line-spanner.cc -- implement Ledger_line_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <map>
#include <set>

#include "item.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "group-interface.hh"
#include "paper-column.hh"

struct Ledger_line_spanner
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  static Stencil brew_ledger_lines (Grob *me,
				    int pos,
				    int interspaces,
				    Real, Real,
				    Interval x_extent,
				    Real left_shorten);

  static bool has_interface (Grob *);
};

Stencil
Ledger_line_spanner::brew_ledger_lines (Grob *staff,
					int pos,
					int interspaces,
					Real halfspace,
					Real ledgerlinethickness,
					Interval x_extent,
					Real left_shorten)
{
  int line_count = ((abs (pos) < interspaces)
		    ? 0
		    : (abs (pos) - interspaces) / 2);
  Stencil stencil;
  if (line_count)
    {
      Real blotdiameter = ledgerlinethickness;
      Interval y_extent
	= Interval (-0.5* (ledgerlinethickness),
		    +0.5* (ledgerlinethickness));
      Stencil proto_ledger_line
	= Lookup::round_filled_box (Box (x_extent, y_extent), blotdiameter);

      x_extent[LEFT] += left_shorten;
      Stencil proto_first_line
	= Lookup::round_filled_box (Box (x_extent, y_extent), blotdiameter);

      Direction dir = (Direction)sign (pos);
      Real offs = (Staff_symbol_referencer::on_staffline (staff, pos))
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

typedef std::map < int, Drul_array<Interval> > Head_extents_map;
typedef std::map < int, Grob *> Column_map;

MAKE_SCHEME_CALLBACK (Ledger_line_spanner, set_spacing_rods, 1);
SCM
Ledger_line_spanner::set_spacing_rods (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));

  // find size of note heads.
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (!staff)
    return SCM_EOL;

  Link_array<Grob> heads (extract_grob_array (me, ly_symbol2scm ("note-heads")));

  if (heads.is_empty ())
    return SCM_EOL;

  Real min_length_fraction
    = robust_scm2double (me->get_property ("minimum-length-fraction"), 0.15);

  Head_extents_map head_extents;
  Column_map columns;

  int interspaces = Staff_symbol::line_count (staff) - 1;
  for (int i = heads.size (); i--;)
    {
      Item *h = dynamic_cast<Item *> (heads[i]);

      int pos = Staff_symbol_referencer::get_rounded_position (h);
      if (pos
	  && abs (pos) > interspaces)
	{
	  Grob *column = h->get_column ();
	  int rank = Paper_column::get_rank (column);

	  Interval head_extent = h->extent (column, X_AXIS);
	  Direction vdir = Direction (sign (pos));
	  if (!vdir)
	    continue;

	  Interval prev_extent;

	  Head_extents_map::iterator j = head_extents.find (rank);
	  if (j != head_extents.end ())
	    prev_extent = (*j).second[vdir];
	  else
	    columns[rank] = column;

	  prev_extent.unite (head_extent);
	  head_extents[rank][vdir] = prev_extent;
	}
    }

  for (Column_map::const_iterator c (columns.begin ()); c != columns.end (); c++)
    {
      Grob *column = (*c).second;
      int rank = (*c).first;

      int next_rank = rank + 2;

      if (head_extents.find (next_rank) != head_extents.end ())
	{
	  Drul_array<Interval> extents_left = head_extents[rank];
	  Drul_array<Interval> extents_right = head_extents[next_rank];

	  Direction d = DOWN;
	  do
	    {
	      if (!extents_right[d].is_empty () && !extents_right[d].is_empty ())
		{
		  Real l1 = extents_right[d].length () * min_length_fraction;
		  Real l2 = extents_left[d].length () * min_length_fraction;

		  Rod rod;
		  rod.distance_ = l1 + l2 + (l1+ l2) / 2.0
		    + extents_left[d][RIGHT]
		    - extents_right[d][LEFT];

		  rod.item_drul_[LEFT] = dynamic_cast<Item *> (column);
		  rod.item_drul_[RIGHT] = dynamic_cast<Item *> (columns[next_rank]);
		  rod.add_to_cols ();
		}
	    }
	  while (flip (&d) != DOWN);
	}
    }

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

typedef std::map < int, Drul_array<Ledger_request> > Ledger_requests;

/*
  TODO: ledger share a lot of info. Lots of room to optimize away common
  use of objects/variables.
*/
MAKE_SCHEME_CALLBACK (Ledger_line_spanner, print, 1);
SCM
Ledger_line_spanner::print (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));
  Link_array<Grob> heads (extract_grob_array (me, ly_symbol2scm ("note-heads")));

  if (heads.is_empty ())
    return SCM_EOL;

  // find size of note heads.
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (!staff)
    return SCM_EOL;

  Real length_fraction
    = robust_scm2double (me->get_property ("length-fraction"), 0.25);

  Stencil ledgers;
  Stencil default_ledger;

  Grob *common[NO_AXES];

  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis a = Axis (i);
      common[a] = common_refpoint_of_array (heads, me, a);
      for (int i = heads.size (); i--;)
	if (Grob *g = unsmob_grob (me->get_property ("accidental-grob")))
	  common[a] = common[a]->common_refpoint (g, a);
    }

  int interspaces = Staff_symbol::line_count (staff) - 1;
  Ledger_requests reqs;
  for (int i = heads.size (); i--;)
    {
      Item *h = dynamic_cast<Item *> (heads[i]);

      int pos = Staff_symbol_referencer::get_rounded_position (h);
      if (pos
	  && abs (pos) > interspaces)
	{
	  Interval head_extent = h->extent (common[X_AXIS], X_AXIS);
	  Interval ledger_extent = head_extent;
	  ledger_extent.widen (length_fraction * head_extent.length ());

	  Direction vdir = Direction (sign (pos));
	  int rank = Paper_column::get_rank (h->get_column ());

	  reqs[rank][vdir].ledger_extent_.unite (ledger_extent);
	  reqs[rank][vdir].head_extent_.unite (head_extent);
	  reqs[rank][vdir].position_
	    = vdir * ((vdir* reqs[rank][vdir].position_) >? (vdir *pos));
	}
    }

  // determine maximum size for non-colliding ledger.
  Real gap = robust_scm2double (me->get_property ("gap"), 0.1);
  Ledger_requests::iterator last (reqs.end ());
  for (Ledger_requests::iterator i (reqs.begin ());
       i != reqs.end (); last = i++)
    {
      if (last == reqs.end ())
	{
	  continue;
	}

      Direction d = DOWN;
      do
	{
	  if (abs (last->second[d].position_) > interspaces
	      && abs (i->second[d].position_) > interspaces)
	    {
	      Real center
		= (last->second[d].head_extent_[RIGHT]
		   + i->second[d].head_extent_[LEFT]) / 2;

	      Direction which = LEFT;
	      do
		{
		  Ledger_request &lr = ((which == LEFT) ? *last : *i).second[d];

		  // due tilt of quarter note-heads
		  bool both
		    = (abs (last->second[d].position_) > interspaces + 1
		       && abs (i->second[d].position_) > interspaces + 1);

		  Real limit = (center + (both? which * gap / 2 : 0));
		  lr.ledger_extent_.elem_ref (-which)
		    = which * (which * lr.ledger_extent_[-which] >? which * limit);
		}
	      while (flip (&which) != LEFT);
	    }
	}
      while (flip (&d) != DOWN);
    }

  // create  ledgers for note heads
  Real ledgerlinethickness
    = Staff_symbol::get_ledger_line_thickness (staff);
  Real halfspace = Staff_symbol::staff_space (staff) / 2;
  for (int i = heads.size (); i--;)
    {
      Item *h = dynamic_cast<Item *> (heads[i]);

      int pos = Staff_symbol_referencer::get_rounded_position (h);
      if (abs (pos) > interspaces + 1)
	{
	  Interval head_size = h->extent (common[X_AXIS], X_AXIS);
	  Interval ledger_size = head_size;
	  ledger_size.widen (ledger_size.length ()* length_fraction);

	  Interval max_size = reqs[Paper_column::get_rank (h->get_column ())][Direction (sign (pos))].ledger_extent_;

	  ledger_size.intersect (max_size);
	  Real left_shorten = 0.0;
	  if (Grob *g = unsmob_grob (h->get_property ("accidental-grob")))
	    {
	      Interval accidental_size = g->extent (common[X_AXIS], X_AXIS);
	      Real d
		= linear_combination (Drul_array<Real> (accidental_size[RIGHT],
							head_size[LEFT]),
				      0.0);

	      left_shorten = (-ledger_size[LEFT] + d) >? 0;

	      /*
		TODO: shorten 2 ledger lines for the case natural +
		downstem.
	      */

	    }

	  ledgers.add_stencil (brew_ledger_lines (staff, pos, interspaces,
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
	       "ledger-line-interface",
	       "This spanner draws the ledger lines of a staff, for note heads that stick out. ",
	       "note-heads thickness minimum-length-fraction length-fraction gap");

struct Ledgered_interface
{
  static bool has_interface (Grob *);
};

ADD_INTERFACE (Ledgered_interface,
	       "ledgered-interface",
	       "Objects that need ledger lines.",
	       "no-ledgers");
