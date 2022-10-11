/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "pointer-group-interface.hh"
#include "paper-column.hh"
#include "interval-set.hh"

#include <map>

using std::map;
using std::vector;

struct Ledger_line_spanner
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
};

static void
set_rods (Drul_array<Interval> const &current_extents,
          Drul_array<Interval> const &previous_extents, Item *current_column,
          Item *previous_column, Real min_length)
{
  for (const auto d : {UP, DOWN})
    {
      if (!current_extents[d].is_empty () && !previous_extents[d].is_empty ())
        {
          Rod rod;
          rod.distance_ = 2 * min_length
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
}

MAKE_SCHEME_CALLBACK (Ledger_line_spanner, set_spacing_rods,
                      "ly:ledger-line-spanner::set-spacing-rods", 1);
SCM
Ledger_line_spanner::set_spacing_rods (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

  // find size of note heads.
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (!staff)
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real min_length_fraction
    = from_scm<double> (get_property (me, "minimum-length-fraction"), 0.15);

  Drul_array<Interval> current_extents;
  Drul_array<Interval> previous_extents;
  Real current_head_width = 0.0;
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
      if (Staff_symbol::ledger_positions (staff, pos).empty ())
        continue;

      /* Ambitus heads can appear out-of-order in heads[],
       * but as part of prefatory matter, they need no rods */
      if (h->internal_has_interface (ly_symbol2scm ("ambitus-interface")))
        continue;

      Item *column = h->get_column ();
      if (current_column != column)
        {
          set_rods (current_extents, previous_extents, current_column,
                    previous_column, current_head_width * min_length_fraction);

          previous_column = current_column;
          current_column = column;
          previous_extents = current_extents;

          current_extents[DOWN].set_empty ();
          current_extents[UP].set_empty ();
          current_head_width = 0.0;
        }

      Interval head_extent = h->extent (column, X_AXIS);
      Direction vdir = Direction (sign (pos));
      if (!vdir)
        continue;

      current_extents[vdir].unite (head_extent);
      current_head_width = std::max (current_head_width, head_extent.length ());
    }

  if (previous_column && current_column)
    set_rods (current_extents, previous_extents, current_column,
              previous_column, current_head_width * min_length_fraction);

  return SCM_UNSPECIFIED;
}

struct Head_data
{
  int position_;
  vector<Real> ledger_positions_;
  Interval head_extent_;
  Interval ledger_extent_;
  Interval accidental_extent_;
  Head_data ()
  {
    position_ = 0;
    head_extent_.set_empty ();
    ledger_extent_.set_empty ();
    accidental_extent_.set_empty ();
  }
};

struct Ledger_request
{
  Interval max_ledger_extent_;
  Interval max_head_extent_;
  int max_position_;
  vector<Head_data> heads_;
  // The map's keys are vertical ledger line positions. The values are
  // vectors of the x-extents of ledger lines.
  std::map<Real, vector<Interval>> ledger_extents_;
  Ledger_request ()
  {
    max_ledger_extent_.set_empty ();
    max_head_extent_.set_empty ();
    max_position_ = 0;
  }
};

typedef std::map<int, Drul_array<Ledger_request>> Ledger_requests;

/*
  TODO: ledger share a lot of info. Lots of room to optimize away
  common use of objects/variables.
*/
MAKE_SCHEME_CALLBACK (Ledger_line_spanner, print,
                      "ly:ledger-line-spanner::print", 1);
SCM
Ledger_line_spanner::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

  // Generate ledger requests from note head properties, etc.
  extract_grob_set (me, "note-heads", heads);

  if (heads.empty ())
    return SCM_EOL;

  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (!staff)
    return SCM_EOL;

  Real halfspace = Staff_symbol::staff_space (staff) / 2;

  Interval staff_extent = staff->extent (staff, Y_AXIS);
  staff_extent *= 1 / halfspace;

  Real length_fraction
    = from_scm<double> (get_property (me, "length-fraction"), 0.25);

  Grob *common_x = common_refpoint_of_array (heads, me, X_AXIS);
  for (vsize i = heads.size (); i--;)
    {
      if (Grob *g = unsmob<Grob> (get_object (heads[i], "accidental-grob")))
        common_x = common_x->common_refpoint (g, X_AXIS);
    }

  Ledger_requests reqs;
  for (vsize i = heads.size (); i--;)
    {
      Item *h = dynamic_cast<Item *> (heads[i]);
      int pos = Staff_symbol_referencer::get_rounded_position (h);
      vector<Real> ledger_positions
        = Staff_symbol::ledger_positions (staff, pos, h);

      // We work with all notes that produce ledgers and any notes that
      // fall outside the staff that do not produce ledgers, such as
      // notes in the first space just beyond the staff.
      if (ledger_positions.size () != 0 || !staff_extent.contains (pos))
        {
          Interval head_extent = h->extent (common_x, X_AXIS);
          Interval ledger_extent = head_extent;
          ledger_extent.widen (length_fraction * head_extent.length ());

          Direction vdir = Direction (sign (pos != 0 ? pos : 1));
          int rank = h->get_column ()->get_rank ();

          reqs[rank][vdir].max_ledger_extent_.unite (ledger_extent);
          reqs[rank][vdir].max_head_extent_.unite (head_extent);
          reqs[rank][vdir].max_position_
            = vdir
              * std::max (vdir * reqs[rank][vdir].max_position_, vdir * pos);
          Head_data hd;
          hd.position_ = pos;
          hd.ledger_positions_ = ledger_positions;
          hd.ledger_extent_ = ledger_extent;
          hd.head_extent_ = head_extent;
          if (Grob *g = unsmob<Grob> (get_object (h, "accidental-grob")))
            hd.accidental_extent_ = g->extent (common_x, X_AXIS);
          reqs[rank][vdir].heads_.push_back (hd);
        }
    }

  if (reqs.size () == 0)
    return SCM_EOL;

  // Iterate through ledger requests and when ledger lines will be
  // too close together horizontally, shorten max_ledger_extent to
  // produce more space between them.
  Real gap = from_scm<double> (get_property (me, "gap"), 0.1);
  Ledger_requests::iterator last (reqs.end ());
  for (Ledger_requests::iterator i (reqs.begin ()); i != reqs.end ();
       last = i++)
    {
      if (last == reqs.end ())
        continue;

      for (const auto d : {DOWN, UP})
        {
          // Some rank--> vdir--> reqs will be 'empty' because notes
          // will not be above AND below the staff for a given rank.
          if (!staff_extent.contains (last->second[d].max_position_)
              && !staff_extent.contains (i->second[d].max_position_))
            {
              // Midpoint between the furthest bounds of the two heads.
              Real center = (last->second[d].max_head_extent_[RIGHT]
                             + i->second[d].max_head_extent_[LEFT])
                            / 2;

              // Do both reqs have notes further than the first space
              // beyond the staff?
              // (due tilt of quarter note-heads)
              /* FIXME */
              bool both = (!staff_extent.contains (
                             last->second[d].max_position_
                             - sign (last->second[d].max_position_))
                           && !staff_extent.contains (
                             i->second[d].max_position_
                             - sign (i->second[d].max_position_)));

              for (const auto which : {LEFT, RIGHT})
                {
                  Ledger_request &lr = ((which == LEFT) ? *last : *i).second[d];

                  Real limit = (center + (both ? which * gap / 2 : 0));
                  lr.max_ledger_extent_.at (-which)
                    = which
                      * std::max (which * lr.max_ledger_extent_[-which],
                                  which * limit);
                }
            }
        }
    }

  // Iterate through ledger requests and the data they have about each
  // note head to generate the final extents for all ledger lines.
  // Note heads of different widths produce different ledger extents.
  for (Ledger_requests::iterator i (reqs.begin ()); i != reqs.end (); i++)
    {
      for (const auto d : {DOWN, UP})
        {
          Ledger_request &lr = i->second[d];
          for (vsize h = 0; h < lr.heads_.size (); h++)
            {
              vector<Real> &ledger_posns = lr.heads_[h].ledger_positions_;
              Interval &ledger_size = lr.heads_[h].ledger_extent_;
              Interval &head_size = lr.heads_[h].head_extent_;
              Interval &acc_extent = lr.heads_[h].accidental_extent_;

              // Limit ledger extents to a maximum to preserve space
              // between ledgers when note heads get close.
              if (!lr.max_ledger_extent_.is_empty ())
                ledger_size.intersect (lr.max_ledger_extent_);

              // Iterate through the ledgers for a given note head.
              for (vsize l = 0; l < ledger_posns.size (); l++)
                {
                  Real lpos = ledger_posns[l];
                  Interval x_extent = ledger_size;

                  // Notes with accidental signs get shorter ledgers.
                  // (Only happens for the furthest note in the column.)
                  if (l == 0 && !acc_extent.is_empty ())
                    {
                      const auto dist
                        = (acc_extent.right () + head_size.left ()) / 2;

                      Real left_shorten
                        = std::max (-ledger_size[LEFT] + dist, 0.0);
                      x_extent[LEFT] += left_shorten;
                      /*
                        TODO: shorten 2 ledger lines for the case
                        natural + downstem.
                      */
                    }
                  if (x_extent.is_empty ())
                    continue;

                  lr.ledger_extents_[lpos].push_back (x_extent);
                }
            }
        }
    }

  // Create the stencil for the ledger line spanner by iterating
  // through the ledger requests and their data on ledger extents.
  Stencil ledgers;
  Real thickness = Staff_symbol::get_ledger_line_thickness (staff);
  Real half_thickness = thickness * 0.5;
  Interval y_extent = Interval (-half_thickness, half_thickness);

  for (Ledger_requests::iterator i (reqs.begin ()); i != reqs.end (); i++)
    {
      for (const auto d : {DOWN, UP})
        {
          std::map<Real, vector<Interval>> &lex = i->second[d].ledger_extents_;
          for (std::map<Real, vector<Interval>>::iterator k = lex.begin ();
               k != lex.end (); k++)
            {
              Real lpos = k->first;
              // When the extents of two ledgers at the same
              // vertical position overlap horizontally, we merge
              // them together to produce a single stencil.  In rare
              // cases they do not overlap and we do not merge them.
              Interval_set merged = Interval_set::interval_union (k->second);
              const vector<Interval> &x_extents = merged.intervals ();

              for (vsize n = 0; n < x_extents.size (); n++)
                {
                  // thickness (ledger line thickness) is the blot diameter
                  Stencil line = Lookup::round_filled_box (
                    Box (x_extents[n], y_extent), thickness);

                  line.translate_axis (lpos * halfspace, Y_AXIS);
                  ledgers.add_stencil (line);
                }
            }
        }
    }
  ledgers.translate_axis (-me->relative_coordinate (common_x, X_AXIS), X_AXIS);
  return ledgers.smobbed_copy ();
}

ADD_INTERFACE (Ledger_line_spanner,
               R"(
This spanner draws the ledger lines of a staff.  This is a separate grob
because it has to process all potential collisions between all note heads.  The
thickness of ledger lines is controlled by the @code{ledger-line-thickness}
property of the @ref{StaffSymbol} grob.
               )",

               /* properties */
               R"(
gap
length-fraction
minimum-length-fraction
note-heads
               )");

struct Ledgered_interface
{
};

ADD_INTERFACE (Ledgered_interface,
               R"(
Objects that need ledger lines, typically note heads.  See also
@ref{ledger-line-spanner-interface}.
               )",

               /* properties */
               R"(
no-ledgers
               )");
