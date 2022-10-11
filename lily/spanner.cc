/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "spanner.hh"

#include "engraver.hh"
#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "pointer-group-interface.hh"
#include "stencil.hh"
#include "system.hh"
#include "warn.hh"

#include <algorithm>
#include <cassert>
#include <vector>

using std::vector;

void
Spanner::do_break_processing ()
{
  //break_into_pieces
  Item *left = get_bound (LEFT);
  Item *right = get_bound (RIGHT);

  if (!left || !right || !is_live ())
    return;

  if (get_system () || is_broken ())
    return;

  if (left == right)
    {
      /*
        If we have a spanner spanning one column, we must break it
        anyway because it might provide a parent for another item.  */
      for (const auto d : {LEFT, RIGHT})
        {
          Item *bound = left->find_prebroken_piece (d);
          if (!bound)
            programming_error ("no broken bound");
          else if (bound->get_system ())
            {
              Spanner *span = clone ();
              span->set_bound (LEFT, bound);
              span->set_bound (RIGHT, bound);

              assert (span->get_system ());
              span->get_system ()->typeset_grob (span);
              broken_intos_.push_back (span);
            }
        }
    }
  else
    {
      System *root = get_root_system (this);
      vector<Item *> break_points = root->broken_col_range (left, right);

      break_points.insert (break_points.begin () + 0, left);
      break_points.push_back (right);

      Slice parent_rank_slice;
      parent_rank_slice.set_full ();

      /*
        Check if our parent in X-direction spans equally wide
        or wider than we do.
      */
      for (const auto a : {X_AXIS, Y_AXIS})
        {
          if (auto *parent = dynamic_cast<Spanner *> (get_parent (a)))
            parent_rank_slice.intersect (
              parent->spanned_column_rank_interval ());
        }

      for (vsize i = 1; i < break_points.size (); i++)
        {
          Drul_array<Item *> bounds;
          bounds[LEFT] = break_points[i - 1];
          bounds[RIGHT] = break_points[i];
          for (const auto d : {LEFT, RIGHT})
            {
              if (!bounds[d]->get_system ())
                bounds[d] = bounds[d]->find_prebroken_piece (-d);
            }

          if (!bounds[LEFT] || !bounds[RIGHT])
            {
              programming_error ("bounds of this piece aren't breakable.");
              continue;
            }

          bool ok = parent_rank_slice.contains (
            bounds[LEFT]->get_column ()->get_rank ());
          ok = ok
               && parent_rank_slice.contains (
                 bounds[RIGHT]->get_column ()->get_rank ());

          if (!ok)
            {
              programming_error (to_string (
                "Spanner `%s' is not fully contained in parent spanner."
                "  Ignoring orphaned part",
                name ().c_str ()));
              continue;
            }

          Spanner *span = clone ();
          span->set_bound (LEFT, bounds[LEFT]);
          span->set_bound (RIGHT, bounds[RIGHT]);

          if (!bounds[LEFT]->get_system () || !bounds[RIGHT]->get_system ()
              || bounds[LEFT]->get_system () != bounds[RIGHT]->get_system ())
            {
              programming_error ("bounds of spanner are invalid");
              span->suicide ();
            }
          else
            {
              bounds[LEFT]->get_system ()->typeset_grob (span);
              broken_intos_.push_back (span);
            }
        }
    }
  std::sort (broken_intos_.begin (), broken_intos_.end (), Spanner::less);
  for (vsize i = broken_intos_.size (); i--;)
    broken_intos_[i]->break_index_ = i;
}

vsize
Spanner::get_break_index () const
{
  return break_index_;
}

Item *
Spanner::get_bound (Direction d) const
{
  if (Item *b = spanned_drul_[d])
    return b;
  else if (internal_has_interface (ly_symbol2scm ("sticky-grob-interface")))
    {
      /* For sticky spanners, there is no point in the engraver cycle where
         the Spanner_tracking_engraver could reliably set the bounds since
         the engravers taking care of the host can set its right bound (and
         even the left bound) as late as the finalize hook, and we don't want
         dependencies on engraver order.  So we handle this at bound retrieval
         time, falling back on the host's bounds if we don't have any. */
      if (Spanner *sp = unsmob<Spanner> (get_object (this, "sticky-host")))
        return sp->get_bound (d);
      else
        programming_error ("sticky spanner's host is not a spanner");
    }
  return nullptr;
}

Drul_array<Item *>
Spanner::get_bounds () const
{
  return {get_bound (LEFT), get_bound (RIGHT)};
}

void
Spanner::set_my_columns ()
{
  for (const auto d : {LEFT, RIGHT})
    {
      Item *b = get_bound (d);
      if (!b->get_system ())
        set_bound (d, b->find_prebroken_piece (-d));
    }
}

Interval_t<int>
Spanner::spanned_column_rank_interval () const
{
  Interval_t<int> iv (0, 0);
  for (const auto d : {LEFT, RIGHT})
    {
      if (Item *b = get_bound (d))
        if (Paper_column *col = b->get_column ())
          iv[d] = col->get_rank ();
    }
  return iv;
}

System_rank_interval
Spanner::spanned_system_rank_interval () const
{
  System_rank_interval rv;

  if (System *st = get_system ())
    rv = System_rank_interval (st->get_rank (), st->get_rank ());
  else
    {
      if (!broken_intos_.empty ())
        {
          rv = System_rank_interval (
            broken_intos_.front ()->get_system ()->get_rank (),
            broken_intos_.back ()->get_system ()->get_rank ());
        }
    }
  return rv;
}

Interval_t<Moment>
Spanner::spanned_time () const
{
  return spanned_time_interval (get_bound (LEFT), get_bound (RIGHT));
}

/*
  Set the items that this spanner spans. If D == LEFT, we also set the
  X-axis parent of THIS to S.

  For example, when a slur crosses a line break, it's broken into two
  pieces.  The second piece shouldn't be positioned relative to the
  original NoteColumn, but rather to the PaperColumn after the break.
*/
void
Spanner::set_bound (Direction d, Grob *g)
{
  // Whether an Item and a Spanner can be linked depends on the specific type
  // of each.  We handle this with two virtual function calls.  This call to
  // the Grob calls back to the most specific Spanner::accepts_as_bound_...()
  // that fits the type of the Grob.
  if (!g->internal_set_as_bound_of_spanner (this, d))
    {
      programming_error (to_string ("cannot set %s as bound of %s",
                                    g->name ().c_str (), name ().c_str ()));
      return;
    }

  spanned_drul_[d] = static_cast<Item *> (g);

  /**
     We check for System to prevent the column -> line_of_score
     -> column -> line_of_score -> etc situation */
  if (d == LEFT && !dynamic_cast<System *> (this))
    /*
      If the X-parent is a spanner, it will be split across linebreaks, too,
      so we shouldn't have to overwrite it with the bound. Also, we need
      original parent for alignment.
      This happens e.g. for MultiMeasureRestNumbers and PercentRepeatCounters.
    */
    if (!dynamic_cast<Spanner *> (get_x_parent ()))
      set_x_parent (g);
}

bool
Spanner::accepts_as_bound_item (const Item *) const
{
  return true;
}

bool
Spanner::accepts_as_bound_paper_column (const Paper_column *col) const
{
  // Spanners in general don't treat Paper_columns specially.
  return Spanner::accepts_as_bound_item (col);
}

Spanner::Spanner (SCM s)
  : Grob (s)
{
  break_index_ = 0;
  add_interface (ly_symbol2scm ("spanner-interface"));
}

Spanner::Spanner (Spanner const &s)
  : Grob (s)
{
  break_index_ = 0;
}

/*
  Certain spanners have pre-computed X values that lie either in
  X-positions or the X key of the alists returned for left-bound-info
  and right-bound-info.  These are calculated to give the real length
  of a spanner (which, because of various padding or overhang properties,
  can extend pass or arrive short of a given bound).  If possible, we
  use these to calculate the spanner's length, and otherwise, we use
  the bound.

  For those writing a new spanner, DO NOT use both X-positions and
  left-bound-info/right-bound-info.
*/
Real
Spanner::spanner_length () const
{
  Interval lr = from_scm (get_property (this, "X-positions"), Interval (1, -1));

  if (lr.is_empty ())
    {
      Drul_array<SCM> bounds (get_property (this, "left-bound-info"),
                              get_property (this, "right-bound-info"));

      for (const auto d : {LEFT, RIGHT})
        lr[d] = from_scm<double> (
          ly_assoc_get (ly_symbol2scm ("X"), bounds[d], SCM_BOOL_F), -d);
    }

  if (lr.is_empty ())
    {
      for (const auto d : {LEFT, RIGHT})
        lr[d] = get_bound (d)->relative_coordinate (0, X_AXIS);
    }

  if (lr.is_empty ())
    programming_error ("spanner with negative length");

  return lr.length ();
}

System *
Spanner::get_system () const
{
  if (Item *left = get_bound (LEFT))
    {
      if (Item *right = get_bound (RIGHT))
        {
          if (auto *const system = left->get_system ())
            {
              if (system == right->get_system ())
                return system;
            }
        }
    }

  return nullptr;
}

Spanner *
Spanner::find_broken_piece (System *l) const
{
  assert (l);
  if (broken_intos_.empty ())
    return nullptr;
  auto l_rank = l->get_rank ();
  Spanner *first = broken_intos_.front ();
  auto first_rank = first->get_system ()->get_rank ();
  if (l_rank >= first_rank)
    {
      auto delta = l_rank - first_rank;
      auto size = broken_intos_.size ();
      if (delta < size)
        return broken_intos_[delta];
    }
  return nullptr;
}

Spanner *
Spanner::broken_neighbor (Direction d) const
{
  auto *const orig = original ();
  if (!orig)
    return 0;

  vsize k = get_break_index ();
  int j = int (k) + d;
  if (j < 0 || vsize (j) >= orig->broken_intos_.size ())
    return 0;

  return orig->broken_intos_[j];
}

bool
Spanner::less (Spanner *const &a, Spanner *const &b)
{
  return a->get_system ()->get_rank () < b->get_system ()->get_rank ();
}

bool
Spanner::is_broken () const
{
  return broken_intos_.size ();
}

void
Spanner::derived_mark () const
{
  scm_gc_mark (pure_property_cache_);
}

/*
  Set left or right bound to IT.

  Warning: caller should ensure that subsequent calls put in ITems
  that are left-to-right ordered.
*/
void
add_bound_item (Spanner *sp, Grob *it)
{
  if (!sp->get_bound (LEFT))
    sp->set_bound (LEFT, it);
  else
    sp->set_bound (RIGHT, it);
}

MAKE_SCHEME_CALLBACK (Spanner, set_spacing_rods, "ly:spanner::set-spacing-rods",
                      1);
SCM
Spanner::set_spacing_rods (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Spanner, smob, 1);
  SCM num_length = get_property (me, "minimum-length");
  SCM broken_length = get_property (me, "minimum-length-after-break");
  if (scm_is_number (num_length) || scm_is_number (broken_length))
    {
      System *root = get_root_system (me);
      const auto bounds = me->get_bounds ();
      if (!bounds[LEFT] || !bounds[RIGHT])
        return SCM_UNSPECIFIED;

      vector<Item *> cols (root->broken_col_range (
        bounds[LEFT]->get_column (), bounds[RIGHT]->get_column ()));

      if (cols.size ())
        {
          Rod r;
          r.item_drul_[LEFT] = me->get_bound (LEFT);
          r.item_drul_[RIGHT] = cols[0]->find_prebroken_piece (LEFT);
          r.distance_ = from_scm<double> (num_length, 0);
          r.add_to_cols ();

          r.item_drul_[LEFT] = cols.back ()->find_prebroken_piece (RIGHT);
          r.item_drul_[RIGHT] = me->get_bound (RIGHT);
          if (scm_is_number (broken_length))
            /*
              r.distance_ may have been modified by add_to_cols ()
              above.  For treatment of minimum-distance-after-break
              consistent with minimum-distance (which will use the
              changed value), we cannot directly reset r.distance_ to
              broken_length.
            */
            r.distance_ += from_scm<double> (broken_length, 0)
                           - from_scm<double> (num_length, 0);
          r.add_to_cols ();
        }

      Rod r;
      /*
        As r is a fresh rod, we can set distance_ with no complication.
      */
      r.distance_ = from_scm<double> (num_length, 0);
      r.item_drul_[LEFT] = me->get_bound (LEFT);
      r.item_drul_[RIGHT] = me->get_bound (RIGHT);
      r.add_to_cols ();

      /*
        We do not know yet if the spanner is going to have a bound that is
        broken. To account for this uncertainty, we add the rod twice:
        once for the central column (see above) and once for the left column
        (see below). As end_rods_ are never used when rods_ are used and vice
        versa, this rod will only be accessed once for each spacing
        configuration before line breaking. Then, as a grob never exists in
        both unbroken and broken forms after line breaking, only one of these
        two rods will be in the column vector used for spacing in
        simple-spacer.cc get_line_configuration.
      */
      if (Item *left_pbp = me->get_bound (RIGHT)->find_prebroken_piece (LEFT))
        {
          r.item_drul_[RIGHT] = left_pbp;
          r.add_to_cols ();
        }
    }

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Spanner, calc_normalized_endpoints,
                      "ly:spanner::calc-normalized-endpoints", 1);
SCM
Spanner::calc_normalized_endpoints (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  SCM result = SCM_EOL;

  Spanner *orig = me->original ();

  orig = orig ? orig : me;

  if (orig->is_broken ())
    {
      Real total_width = 0.0;
      vector<Real> span_data;

      for (vsize i = 0; i < orig->broken_intos_.size (); i++)
        span_data.push_back (orig->broken_intos_[i]->spanner_length ());

      vector<Interval> unnormalized_endpoints;

      for (vsize i = 0; i < span_data.size (); i++)
        {
          unnormalized_endpoints.push_back (
            Interval (total_width, total_width + span_data[i]));
          total_width += span_data[i];
        }

      for (vsize i = 0; i < unnormalized_endpoints.size (); i++)
        {
          SCM t = to_scm (1 / total_width * unnormalized_endpoints[i]);
          set_property (orig->broken_intos_[i], "normalized-endpoints", t);
          if (me->get_break_index () == i)
            result = t;
        }
    }
  else
    {
      result = scm_cons (to_scm (0.0), to_scm (1.0));
      set_property (orig, "normalized-endpoints", result);
    }

  return result;
}

MAKE_SCHEME_CALLBACK (Spanner, bounds_width, "ly:spanner::bounds-width", 1);
SCM
Spanner::bounds_width (SCM grob)
{
  auto *const me = unsmob<Spanner> (grob);
  auto *const lb = me->get_bound (LEFT);
  auto *const rb = me->get_bound (RIGHT);
  auto *const common = lb->common_refpoint (rb, X_AXIS);

  Interval w (lb->relative_coordinate (common, X_AXIS),
              rb->relative_coordinate (common, X_AXIS));

  w -= me->relative_coordinate (common, X_AXIS);

  return to_scm (w);
}

MAKE_SCHEME_CALLBACK (Spanner, kill_zero_spanned_time,
                      "ly:spanner::kill-zero-spanned-time", 1);
SCM
Spanner::kill_zero_spanned_time (SCM grob)
{
  Spanner *me = unsmob<Spanner> (grob);
  /*
    Remove the line or hairpin at the start of the line.  For
    piano voice indicators, it makes no sense to have them at
    the start of the line.

    I'm not sure what the official rules for glissandi are, but
    usually the 2nd note of the glissando is "exact", so when playing
    from the start of the line, there is no need to glide.

    From a typographical p.o.v. this makes sense, since the amount of
    space left of a note at the start of a line is very small.

    --hwn.

  */
  if (me->get_bound (LEFT)->break_status_dir ())
    {
      Interval_t<Moment> moments = me->spanned_time ();
      moments[LEFT].grace_part_ = 0;
      if (moments.length () == Moment (0, 0))
        me->suicide ();
    }

  return SCM_UNSPECIFIED;
}

// The pure property cache is indexed by (name start . end), where name is
// a symbol, and start and end are numbers referring to the starting and
// ending column ranks of the current line.
static SCM
make_pure_property_cache_key (SCM sym, vsize start, vsize end)
{
  return scm_cons2 (sym, to_scm (start), to_scm (end));
}

SCM
Spanner::get_cached_pure_property (SCM sym, vsize start, vsize end)
{
  if (SCM_UNBNDP (pure_property_cache_))
    return SCM_UNDEFINED;

  return scm_hash_ref (pure_property_cache_,
                       make_pure_property_cache_key (sym, start, end),
                       SCM_UNDEFINED);
}

void
Spanner::cache_pure_property (SCM sym, vsize start, vsize end, SCM val)
{
  if (SCM_UNBNDP (pure_property_cache_))
    pure_property_cache_ = scm_c_make_hash_table (17);

  scm_hash_set_x (pure_property_cache_,
                  make_pure_property_cache_key (sym, start, end), val);
}

Spanner *
Spanner::make_sticky_same_type (Engraver *eng, SCM type, SCM cause,
                                char const *file, int line, char const *fun)
{
  Spanner *g = eng->internal_make_spanner (type, cause, file, line, fun);
  // Delegate ending the sticky spanner to the Spanner_tracking_engraver.
  // The bounds are inherited implicitly.
  return g;
}

ADD_INTERFACE (Spanner,
               R"(
Some objects are horizontally spanned between objects.  For example, slurs,
beams, ties, etc.  These grobs form a subtype called @code{Spanner}.  All
spanners have two span points (these must be @code{Item} objects), one on the
left and one on the right.  The left bound is also the X@tie{}reference point
of the spanner.
               )",

               /* properties */
               R"(
normalized-endpoints
minimum-length
minimum-length-after-break
spanner-broken
spanner-id
to-barline
               )");
