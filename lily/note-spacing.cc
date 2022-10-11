/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "note-spacing.hh"

#include "accidental-placement.hh"
#include "break-align-interface.hh"
#include "directional-element-interface.hh"
#include "grob-array.hh"
#include "moment.hh"
#include "note-column.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "spacing-interface.hh"
#include "staff-spacing.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "warn.hh"

using std::vector;

/*
  Adjust the ideal and minimum distance between note columns,
  based on the notehead size, skylines, and optical illusions.
*/
Spring
Note_spacing::get_spacing (Grob *me, Item *right_col, Spring base,
                           Real increment)
{
  vector<Item *> note_columns = Spacing_interface::left_note_columns (me);
  Real left_head_end = 0;

  for (vsize i = 0; i < note_columns.size (); i++)
    {
      SCM r = get_object (note_columns[i], "rest");
      Grob *g = unsmob<Grob> (r);
      Grob *col = note_columns[i]->get_column ();

      if (!g)
        g = Note_column::first_head (note_columns[i]);

      /*
        Ugh. If Stem is switched off, we don't know what the
        first note head will be.
      */
      if (g)
        {
          if (!g->has_in_ancestry (col, X_AXIS))
            programming_error (
              "Note_spacing::get_spacing (): Common refpoint incorrect");
          else
            left_head_end = g->extent (col, X_AXIS)[RIGHT];
        }
    }

  /*
    The main factor that determines the amount of space is the width of the
    note head (or the rest). For example, a quarter rest gets almost 0.5 ss
    less horizontal space than a note.
  */
  Real ideal = base.ideal_distance () - increment + left_head_end;
  Drul_array<Skyline> skys = Spacing_interface::skylines (me, right_col);
  Real distance = skys[LEFT].distance (
    skys[RIGHT], from_scm<double> (
                   get_property (right_col, "skyline-vertical-padding"), 0.0));
  Real min_dist = std::max (0.0, distance);
  base.set_min_distance (min_dist);

  /* If we have a NonMusical column on the right, we measure the ideal distance
     to the bar-line (if present), not the start of the column. */
  if (!Paper_column::is_musical (right_col) && !skys[RIGHT].is_empty ()
      && from_scm<bool> (get_property (me, "space-to-barline")))
    {
      Grob *staff_bar_group = nullptr;
      if (auto *break_alignment
          = unsmob<Item> (get_object (right_col, "break-alignment")))
        {
          staff_bar_group
            = Break_alignment_interface::find_nonempty_break_align_group (
              break_alignment, ly_symbol2scm ("staff-bar"));
        }

      if (staff_bar_group)
        ideal -= staff_bar_group->extent (right_col, X_AXIS)[LEFT];
      else
        {
          /* Measure ideal distance to the right side of the NonMusical column
             but keep at least half the gap we would have had to a note */
          Real min_desired_space = (ideal + min_dist) / 2.0;
          ideal -= right_col->extent (right_col, X_AXIS)[RIGHT];
          ideal = std::max (ideal, min_desired_space);
        }
    }

  stem_dir_correction (me, right_col, increment, &ideal);

  base.set_ideal_distance (std::max (0.0, ideal));
  return base;
}

static Real
knee_correction (Grob *note_spacing, Grob *right_stem, Real increment)
{
  Real note_head_width = increment;
  Item *head
    = right_stem ? dynamic_cast<Item *> (Stem::support_head (right_stem)) : 0;

  if (head)
    {
      Interval head_extent = head->extent (head->get_column (), X_AXIS);

      if (!head_extent.is_empty ())
        note_head_width = head_extent[RIGHT];

      note_head_width -= Stem::thickness (right_stem);
    }

  return -note_head_width * get_grob_direction (right_stem)
         * from_scm<double> (
           get_property (note_spacing, "knee-spacing-correction"), 0);
}

static Real
different_directions_correction (Grob *note_spacing,
                                 Drul_array<Interval> stem_posns,
                                 Direction left_stem_dir)
{
  Real ret = 0.0;
  Interval intersect = stem_posns[LEFT];
  intersect.intersect (stem_posns[RIGHT]);

  if (!intersect.is_empty ())
    {
      ret = abs (intersect.length ());

      /*
        Ugh. 7 is hardcoded.
      */
      ret = std::min (ret / 7, 1.0) * left_stem_dir
            * from_scm<double> (
              get_property (note_spacing, "stem-spacing-correction"), 0);
    }
  return ret;
}

static Real
same_direction_correction (Grob *note_spacing, Drul_array<Interval> head_posns)
{
  /*
    Correct for the following situation:

    X      X
    |      |
    |      |
    |   X  |
    |  |   |
    ========

    ^ move the center one to the left.


    this effect seems to be much more subtle than the
    stem-direction stuff (why?), and also does not scale with the
    difference in stem length.

  */

  Interval hp = head_posns[LEFT];
  hp.intersect (head_posns[RIGHT]);
  if (!hp.is_empty ())
    return 0;

  Direction lowest
    = (head_posns[LEFT][DOWN] > head_posns[RIGHT][UP]) ? RIGHT : LEFT;

  Real delta = head_posns[-lowest][DOWN] - head_posns[lowest][UP];
  Real corr = from_scm<double> (
    get_property (note_spacing, "same-direction-correction"), 0);

  return (delta > 1) ? -lowest * corr : 0;
}

/*
  Correct for optical illusions. See [Wanske] p. 138. The combination
  up-stem + down-stem should get extra space, the combination
  down-stem + up-stem less.

  TODO: have to check whether the stems are in the same staff.
*/
void
Note_spacing::stem_dir_correction (Grob *me, Item *rcolumn, Real increment,
                                   Real *space)
{
  Drul_array<Direction> stem_dirs;
  Drul_array<Interval> stem_posns;
  Drul_array<Interval> head_posns;
  Drul_array<SCM> props (get_object (me, "left-items"),
                         get_object (me, "right-items"));

  Drul_array<Spanner *> beams_drul;
  Drul_array<Grob *> stems_drul;

  Interval intersect;
  Interval bar_xextent;
  Interval bar_yextent;

  bool acc_right = false;

  Grob *bar = Spacing_interface::extremal_break_aligned_grob (
    me, RIGHT, rcolumn->break_status_dir (), &bar_xextent);
  if (bar && dynamic_cast<Item *> (bar)->get_column () == rcolumn)
    bar_yextent = Staff_spacing::bar_y_positions (bar);

  for (const auto d : {LEFT, RIGHT})
    {
      vector<Grob *> const &items (ly_scm2link_array (props[d]));
      for (vsize i = 0; i < items.size (); i++)
        {
          Item *it = dynamic_cast<Item *> (items[i]);
          if (!has_interface<Note_column> (it))
            continue;
          if (d == RIGHT && it->get_column () != rcolumn)
            continue;

          /*
            Find accidentals which are sticking out of the right side.
          */
          if (d == RIGHT)
            acc_right = acc_right || Note_column::accidentals (it);

          Grob *stem = Note_column::get_stem (it);

          if (!stem || !stem->is_live () || Stem::is_invisible (stem))
            return;

          stems_drul[d] = stem;
          beams_drul[d] = Stem::get_beam (stem);

          Direction stem_dir = get_grob_direction (stem);
          if (stem_dirs[d] && stem_dirs[d] != stem_dir)
            return;

          stem_dirs[d] = stem_dir;

          /*
            Correction doesn't seem appropriate  when there is a large flag
            hanging from the note.
          */
          if (d == LEFT && Stem::duration_log (stem) > 2
              && !Stem::get_beam (stem))
            return;

          Interval hp = Stem::head_positions (stem);
          if (!hp.is_empty ())
            {
              Real ss = Staff_symbol_referencer::staff_space (stem);
              stem_posns[d] = stem->pure_y_extent (stem, 0, INT_MAX) * (2 / ss);
              head_posns[d].unite (hp);
            }
        }
    }

  Real correction = 0.0;

  if (!bar_yextent.is_empty ())
    {
      stem_dirs[RIGHT] = -stem_dirs[LEFT];
      stem_posns[RIGHT] = bar_yextent;
      stem_posns[RIGHT] *= 2;
    }

  if (directed_opposite (stem_dirs[LEFT], stem_dirs[RIGHT]))
    {
      if (beams_drul[LEFT] && beams_drul[LEFT] == beams_drul[RIGHT])
        {
          correction = knee_correction (me, stems_drul[RIGHT], increment);
        }
      else
        {
          correction
            = different_directions_correction (me, stem_posns, stem_dirs[LEFT]);

          if (!bar_yextent.is_empty ())
            correction *= 0.5;
        }
    }
  /*
    Only apply same direction correction if there are no
    accidentals sticking out of the right hand side.
  */
  else if (directed_same (stem_dirs[LEFT], stem_dirs[RIGHT]) && !acc_right)
    correction = same_direction_correction (me, head_posns);

  *space += correction;

  /* there used to be a correction for bar_xextent () here, but
     it's unclear what that was good for ?
  */
}

ADD_INTERFACE (Note_spacing,
               R"(
This object calculates spacing wishes for individual voices.
               )",

               /* properties */
               R"(
knee-spacing-correction
left-items
right-items
same-direction-correction
stem-spacing-correction
space-to-barline
               )");
