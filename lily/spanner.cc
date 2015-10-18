/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "libc-extension.hh"
#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "pointer-group-interface.hh"
#include "stencil.hh"
#include "system.hh"
#include "warn.hh"

Grob *
Spanner::clone () const
{
  return new Spanner (*this);
}

void
Spanner::do_break_processing ()
{
  //break_into_pieces
  Item *left = spanned_drul_[LEFT];
  Item *right = spanned_drul_[RIGHT];

  if (!left || !right)
    return;

  if (get_system () || is_broken ())
    return;

  if (left == right)
    {
      /*
        If we have a spanner spanning one column, we must break it
        anyway because it might provide a parent for another item.  */
      for (LEFT_and_RIGHT (d))
        {
          Item *bound = left->find_prebroken_piece (d);
          if (!bound)
            programming_error ("no broken bound");
          else if (bound->get_system ())
            {
              Spanner *span = dynamic_cast<Spanner *> (clone ());
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
      for (int a = X_AXIS; a < NO_AXES; a++)
        {
          if (Spanner *parent = dynamic_cast<Spanner *> (get_parent ((Axis)a)))
            parent_rank_slice.intersect (parent->spanned_rank_interval ());
        }

      for (vsize i = 1; i < break_points.size (); i++)
        {
          Drul_array<Item *> bounds;
          bounds[LEFT] = break_points[i - 1];
          bounds[RIGHT] = break_points[i];
          for (LEFT_and_RIGHT (d))
            {
              if (!bounds[d]->get_system ())
                bounds[d] = bounds[d]->find_prebroken_piece (- d);
            }

          if (!bounds[LEFT] || ! bounds[RIGHT])
            {
              programming_error ("bounds of this piece aren't breakable.");
              continue;
            }

          bool ok = parent_rank_slice.contains (bounds[LEFT]->get_column ()->get_rank ());
          ok = ok && parent_rank_slice.contains (bounds[RIGHT]->get_column ()->get_rank ());

          if (!ok)
            {
              programming_error (to_string ("Spanner `%s' is not fully contained in parent spanner."
                                            "  Ignoring orphaned part",
                                            name ().c_str ()));
              continue;
            }

          Spanner *span = dynamic_cast<Spanner *> (clone ());
          span->set_bound (LEFT, bounds[LEFT]);
          span->set_bound (RIGHT, bounds[RIGHT]);

          if (!bounds[LEFT]->get_system ()
              || !bounds[RIGHT]->get_system ()
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
  vector_sort (broken_intos_, Spanner::less);
  for (vsize i = broken_intos_.size (); i--;)
    broken_intos_[i]->break_index_ = i;
}

vsize
Spanner::get_break_index () const
{
  return break_index_;
}

void
Spanner::set_my_columns ()
{
  for (LEFT_and_RIGHT (d))
    {
      if (!spanned_drul_[d]->get_system ())
        set_bound (d, spanned_drul_[d]->find_prebroken_piece ((Direction) - d));
    }
}

Interval_t<int>
Spanner::spanned_rank_interval () const
{
  Interval_t<int> iv (0, 0);

  if (spanned_drul_[LEFT] && spanned_drul_[LEFT]->get_column ())
    iv[LEFT] = spanned_drul_[LEFT]->get_column ()->get_rank ();
  if (spanned_drul_[RIGHT] && spanned_drul_[RIGHT]->get_column ())
    iv[RIGHT] = spanned_drul_[RIGHT]->get_column ()->get_rank ();
  return iv;
}

Interval_t<Moment>
Spanner::spanned_time () const
{
  return spanned_time_interval (spanned_drul_[LEFT],
                                spanned_drul_[RIGHT]);
}

Item *
Spanner::get_bound (Direction d) const
{
  return spanned_drul_[d];
}

/*
  Set the items that this spanner spans. If D == LEFT, we also set the
  X-axis parent of THIS to S.

  For example, when a slur crosses a line break, it's broken into two
  pieces.  The second piece shouldn't be positioned relative to the
  original NoteColumn, but rather to the PaperColumn after the break.
*/
void
Spanner::set_bound (Direction d, Grob *s)
{
  Item *i = dynamic_cast<Item *> (s);
  if (!i)
    {
      programming_error ("must have Item for spanner bound of " + name ());
      return;
    }

  spanned_drul_[d] = i;

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
    if (!dynamic_cast <Spanner *> (get_parent (X_AXIS)))
      set_parent (i, X_AXIS);

  /*
    Signal that this column needs to be kept alive. They need to be
    kept alive to have meaningful position and linebreaking.

    [maybe we should try keeping all columns alive?, and perhaps
    inherit position from their (non-)musical brother]
  */
  if (dynamic_cast<Paper_column *> (i))
    Pointer_group_interface::add_grob (i, ly_symbol2scm ("bounded-by-me"), this);
}

Spanner::Spanner (SCM s)
  : Grob (s)
{
  break_index_ = 0;
  spanned_drul_.set (0, 0);
  pure_property_cache_ = SCM_UNDEFINED;
}

Spanner::Spanner (Spanner const &s)
  : Grob (s)
{
  spanned_drul_.set (0, 0);
  pure_property_cache_ = SCM_UNDEFINED;
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
  Interval lr = robust_scm2interval (get_property ("X-positions"),
                                     Interval (1, -1));

  if (lr.is_empty ())
    {
      Drul_array<SCM> bounds (get_property ("left-bound-info"),
                              get_property ("right-bound-info"));

      for (LEFT_and_RIGHT (d))
        lr[d] = robust_scm2double (ly_assoc_get (ly_symbol2scm ("X"),
                                                 bounds[d], SCM_BOOL_F), -d);
    }

  if (lr.is_empty ())
    {
      for (LEFT_and_RIGHT (d))
        lr[d] = spanned_drul_[d]->relative_coordinate (0, X_AXIS);
    }

  if (lr.is_empty ())
    programming_error ("spanner with negative length");

  return lr.length ();
}

System *
Spanner::get_system () const
{
  if (!spanned_drul_[LEFT] || !spanned_drul_[RIGHT])
    return 0;
  if (spanned_drul_[LEFT]->get_system () != spanned_drul_[RIGHT]->get_system ())
    return 0;
  return spanned_drul_[LEFT]->get_system ();
}

Grob *
Spanner::find_broken_piece (System *l) const
{
  vsize idx = binary_search (broken_intos_, (Spanner *) l, Spanner::less);
  if (idx != VPOS)
    return broken_intos_ [idx];
  return 0;
}

Spanner *
Spanner::broken_neighbor (Direction d) const
{
  if (!original_)
    return 0;

  vsize k = get_break_index ();
  Spanner *orig = dynamic_cast<Spanner *> (original_);
  int j = int (k) + d;
  if (j < 0 || vsize (j) >= orig->broken_intos_.size ())
    return 0;

  return orig->broken_intos_[j];
}

int
Spanner::compare (Spanner *const &p1, Spanner *const &p2)
{
  return p1->get_system ()->get_rank () - p2->get_system ()->get_rank ();
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

  for (LEFT_and_RIGHT (d))
    if (spanned_drul_[d])
      scm_gc_mark (spanned_drul_[d]->self_scm ());
  ;

  for (vsize i = broken_intos_.size (); i--;)
    scm_gc_mark (broken_intos_[i]->self_scm ());
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

MAKE_SCHEME_CALLBACK (Spanner, set_spacing_rods, 1);
SCM
Spanner::set_spacing_rods (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  SCM num_length = me->get_property ("minimum-length");
  SCM broken_length = me->get_property ("minimum-length-after-break");
  if (scm_is_number (num_length)
     || scm_is_number (broken_length))
    {
      Spanner *sp = dynamic_cast<Spanner *> (me);
      System *root = get_root_system (me);
      Drul_array<Item *> bounds (sp->get_bound (LEFT),
                                 sp->get_bound (RIGHT));
      if (!bounds[LEFT] || !bounds[RIGHT])
        return SCM_UNSPECIFIED;

      vector<Item *> cols (root->broken_col_range (bounds[LEFT]->get_column (),
                                                   bounds[RIGHT]->get_column ()));

      if (cols.size ())
        {
          Rod r;
          r.item_drul_[LEFT] = sp->get_bound (LEFT);
          r.item_drul_[RIGHT] = cols[0]->find_prebroken_piece (LEFT);
          r.distance_ = robust_scm2double (num_length, 0);
          r.add_to_cols ();

          r.item_drul_[LEFT] = cols.back ()->find_prebroken_piece (RIGHT);
          r.item_drul_[RIGHT] = sp->get_bound (RIGHT);
          if (scm_is_number (broken_length))
            /*
              r.distance_ may have been modified by add_to_cols ()
              above.  For treatment of minimum-distance-after-break
              consistent with minimum-distance (which will use the
              changed value), we cannot directly reset r.distance_ to
              broken_length.
            */
            r.distance_ += robust_scm2double (broken_length, 0) -
              robust_scm2double (num_length, 0);
          r.add_to_cols ();
        }

      Rod r;
      /*
        As r is a fresh rod, we can set distance_ with no complication.
      */
      r.distance_ = robust_scm2double (num_length, 0);
      r.item_drul_[LEFT] = sp->get_bound (LEFT);
      r.item_drul_[RIGHT] = sp->get_bound (RIGHT);
      r.add_to_cols ();

      /*
        We do not know yet if the spanner is going to have a bound that is
        broken. To account for this uncertainty, we add the rod twice:
        once for the central column (see above) and once for the left column
        (see below). As end_rods_ are never used when rods_ are used and vice
        versa, this rod will only be accessed once for each spacing
        configuraiton before line breaking. Then, as a grob never exists in
        both unbroken and broken forms after line breaking, only one of these
        two rods will be in the column vector used for spacing in
        simple-spacer.cc get_line_confugration.
      */
      if (Item *left_pbp = sp->get_bound (RIGHT)->find_prebroken_piece (LEFT))
        {
          r.item_drul_[RIGHT] = left_pbp;
          r.add_to_cols ();
        }
    }

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Spanner, calc_normalized_endpoints, 1);
SCM
Spanner::calc_normalized_endpoints (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  SCM result = SCM_EOL;

  Spanner *orig = dynamic_cast<Spanner *> (me->original ());

  orig = orig ? orig : me;

  if (orig->is_broken ())
    {
      Real total_width = 0.0;
      vector<Real> span_data;

      if (!orig->is_broken ())
        span_data.push_back (orig->spanner_length ());
      else
        for (vsize i = 0; i < orig->broken_intos_.size (); i++)
          span_data.push_back (orig->broken_intos_[i]->spanner_length ());

      vector<Interval> unnormalized_endpoints;

      for (vsize i = 0; i < span_data.size (); i++)
        {
          unnormalized_endpoints.push_back (Interval (total_width, total_width + span_data[i]));
          total_width += span_data[i];
        }

      for (vsize i = 0; i < unnormalized_endpoints.size (); i++)
        {
          SCM t = ly_interval2scm (1 / total_width * unnormalized_endpoints[i]);
          orig->broken_intos_[i]->set_property ("normalized-endpoints", t);
          if (me->get_break_index () == i)
            result = t;
        }
    }
  else
    {
      result = scm_cons (scm_from_double (0.0), scm_from_double (1.0));
      orig->set_property ("normalized-endpoints", result);
    }

  return result;
}

MAKE_SCHEME_CALLBACK (Spanner, bounds_width, 1);
SCM
Spanner::bounds_width (SCM grob)
{
  Spanner *me = unsmob<Spanner> (grob);

  Grob *common = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);

  Interval w (me->get_bound (LEFT)->relative_coordinate (common, X_AXIS),
              me->get_bound (RIGHT)->relative_coordinate (common, X_AXIS));

  w -= me->relative_coordinate (common, X_AXIS);

  return ly_interval2scm (w);
}

MAKE_SCHEME_CALLBACK (Spanner, kill_zero_spanned_time, 1);
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
      moments [LEFT].grace_part_ = 0;
      if (moments.length () == Moment (0, 0))
        me->suicide ();
    }

  return SCM_UNSPECIFIED;
}

SCM
Spanner::get_cached_pure_property (SCM sym, int start, int end)
{
  // The pure property cache is indexed by (name start . end), where name is
  // a symbol, and start and end are numbers referring to the starting and
  // ending column ranks of the current line.
  if (scm_is_false (scm_hash_table_p (pure_property_cache_)))
    return SCM_UNDEFINED;

  SCM key = scm_cons (sym, scm_cons (scm_from_int (start), scm_from_int (end)));
  return scm_hash_ref (pure_property_cache_, key, SCM_UNDEFINED);
}

void
Spanner::cache_pure_property (SCM sym, int start, int end, SCM val)
{
  if (scm_is_false (scm_hash_table_p (pure_property_cache_)))
    pure_property_cache_ = scm_c_make_hash_table (17);

  SCM key = scm_cons (sym, scm_cons (scm_from_int (start), scm_from_int (end)));
  scm_hash_set_x (pure_property_cache_, key, val);
}

ADD_INTERFACE (Spanner,
               "Some objects are horizontally spanned between objects.  For"
               " example, slurs, beams, ties, etc.  These grobs form a subtype"
               " called @code{Spanner}.  All spanners have two span points"
               " (these must be @code{Item} objects), one on the left and one"
               " on the right.  The left bound is also the X@tie{}reference"
               " point of the spanner.",

               /* properties */
               "normalized-endpoints "
               "minimum-length "
               "minimum-length-after-break "
               "spanner-broken "
               "spanner-id "
               "to-barline "
              );
