/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "axis-group-interface.hh"
#include "directional-element-interface.hh"
#include "dot-column.hh"
#include "dot-configuration.hh"
#include "dot-formatting-problem.hh"
#include "grob.hh"
#include "note-head.hh"
#include "pointer-group-interface.hh"
#include "rest.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <map>
#include <set>
#include <vector>

using std::set;
using std::vector;

MAKE_SCHEME_CALLBACK (Dot_column, calc_positioning_done,
                      "ly:dot-column::calc-positioning-done", 1);
SCM
Dot_column::calc_positioning_done (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  /*
    Trigger note collision resolution first, since that may kill off
    dots when merging.
  */
  if (Grob *collision = unsmob<Grob> (get_object (me, "note-collision")))
    (void) get_property (collision, "positioning-done");

  set_property (me, "positioning-done", SCM_BOOL_T);

  vector<Grob *> dots = extract_grob_array (me, "dots");

  vector<Grob *> parent_stems;
  Real ss = 0;

  Grob *commonx = me;
  for (vsize i = 0; i < dots.size (); i++)
    {
      Grob *n = dots[i]->get_y_parent ();
      commonx = n->common_refpoint (commonx, X_AXIS);

      if (Grob *stem = unsmob<Grob> (get_object (n, "stem")))
        {
          commonx = stem->common_refpoint (commonx, X_AXIS);

          if (Stem::first_head (stem) == n)
            parent_stems.push_back (stem);
        }
    }

  vector<Box> boxes;
  set<Grob *> stems;

  extract_grob_set (me, "side-support-elements", support);

  Interval base_x;
  for (vsize i = 0; i < parent_stems.size (); i++)
    base_x.unite (Stem::first_head (parent_stems[i])->extent (commonx, X_AXIS));

  // TODO: could this be refactored using side-position-interface?
  for (vsize i = 0; i < support.size (); i++)
    {
      Grob *s = support[i];
      if (!ss)
        ss = Staff_symbol_referencer::staff_space (s);

      /* can't inspect Y extent of rest.

         Rest collisions should wait after line breaking.
      */
      Interval y;
      if (has_interface<Rest> (s))
        {
          base_x.unite (s->extent (commonx, X_AXIS));
          continue;
        }
      else if (has_interface<Stem> (s))
        {
          Real y1 = Stem::head_positions (s)[-get_grob_direction (s)];
          Real y2 = y1 + get_grob_direction (s) * 7;

          y.add_point (y1);
          y.add_point (y2);

          stems.insert (s);
        }
      else if (has_interface<Note_head> (s))
        y = Interval (-1.1, 1.1);
      else
        {
          programming_error ("unknown grob in dot col support");
          continue;
        }

      y += Staff_symbol_referencer::get_position (s);

      Box b (s->extent (commonx, X_AXIS), y);
      boxes.push_back (b);

      if (Grob *stem = unsmob<Grob> (get_object (s, "stem")))
        stems.insert (stem);
    }

  for (set<Grob *>::const_iterator i (stems.begin ()); i != stems.end (); i++)
    {
      Grob *stem = (*i);
      Grob *flag = Stem::flag (stem);
      if (flag)
        {
          Grob *commony = stem->common_refpoint (flag, Y_AXIS);
          Interval y = flag->extent (commony, Y_AXIS) * (2 / ss);
          Interval x = flag->extent (commonx, X_AXIS);

          boxes.push_back (Box (x, y));
        }
    }

  /*
    The use of pure_position_less and pure_get_rounded_position below
    are due to the fact that this callback is called before line breaking
    occurs.  Because dots' actual Y posiitons may be linked to that of
    beams (dots are attached to rests, which are shifted to avoid beams),
    we instead must use their pure Y positions.
  */
  std::sort (dots.begin (), dots.end (), pure_position_less);

  SCM chord_dots_limit = get_property (me, "chord-dots-limit");
  if (scm_is_number (chord_dots_limit))
    {
      // Sort dots by stem, then check for dots above the limit for each stem
      vector<vector<Grob *>> dots_each_stem (parent_stems.size ());
      for (vsize i = 0; i < dots.size (); i++)
        if (Grob *stem
            = unsmob<Grob> (get_object (dots[i]->get_y_parent (), "stem")))
          for (vsize j = 0; j < parent_stems.size (); j++)
            if (stem == parent_stems[j])
              {
                dots_each_stem[j].push_back (dots[i]);
                break;
              }
      for (vsize j = 0; j < parent_stems.size (); j++)
        {
          Interval chord = Stem::head_positions (parent_stems[j]);
          vsize total_room = (static_cast<size_t> (chord.length ()) + 2
                              + scm_to_size_t (chord_dots_limit))
                             / 2;
          vsize total_dots = dots_each_stem[j].size ();
          // remove excessive dots from the ends of the stem
          for (vsize first_dot = 0; total_dots > total_room; total_dots--)
            if (0 == (total_dots - total_room) % 2)
              dots_each_stem[j][first_dot++]->suicide ();
            else
              dots_each_stem[j][first_dot + total_dots - 1]->suicide ();
        }
    }

  for (vsize i = dots.size (); i--;)
    {
      if (!dots[i]->is_live ())
        dots.erase (dots.begin () + i);
      else
        // Undo any fake translations that were done in add_head.
        dots[i]->translate_axis (-dots[i]->relative_coordinate (me, X_AXIS),
                                 X_AXIS);
    }

  Dot_formatting_problem problem (boxes, base_x);

  Dot_configuration cfg (problem);
  for (vsize i = 0; i < dots.size (); i++)
    {
      Dot_position dp;
      dp.dot_ = dots[i];

      Grob *note = dots[i]->get_y_parent ();
      if (note)
        {
          if (has_interface<Note_head> (note))
            dp.dir_ = from_scm<Direction> (get_property (dp.dot_, "direction"));

          dp.x_extent_ = note->extent (commonx, X_AXIS);
        }

      int p = Staff_symbol_referencer::pure_get_rounded_position (dp.dot_);

      /* icky, since this should go via a Staff_symbol_referencer
         offset callback but adding a dot overwrites Y-offset. */
      p += static_cast<int> (
        from_scm<double> (get_property (dp.dot_, "staff-position"), 0.0));
      dp.pos_ = p;

      cfg.remove_collision (p);
      cfg[p] = dp;
      if (Staff_symbol_referencer::on_line (dp.dot_, p)
          && !scm_is_eq (get_property (dp.dot_, "style"),
                         ly_symbol2scm ("kievan")))
        cfg.remove_collision (p);
    }

  for (const auto &ent : cfg) // Junkme?
    Staff_symbol_referencer::pure_set_position (ent.second.dot_, ent.first);

  me->translate_axis (cfg.x_offset ()
                        - me->relative_coordinate (commonx, X_AXIS)
                        + from_scm<Real> (get_property (me, "padding"), 0),
                      X_AXIS);
  return SCM_BOOL_T;
}

void
Dot_column::add_head (Grob *me, Grob *head)
{
  Grob *d = unsmob<Grob> (get_object (head, "dot"));
  if (d)
    {
      Side_position_interface::add_support (me, head);

      Pointer_group_interface::add_grob (me, ly_symbol2scm ("dots"), d);
      set_property (d, "Y-offset", Grob::x_parent_positioning_proc);
      // Dot formatting requests the Y-offset, which for rests may
      // trigger post-linebreak callbacks.  On the other hand, we need the
      // correct X-offset of the dots for horizontal collision avoidance.
      // The translation here is undone in calc_positioning_done, where we
      // do the X-offset properly.
      // TODO: this seems very hacky.  We should try to find something better.
      if (has_interface<Rest> (head))
        {
          d->translate_axis (head->extent (head, X_AXIS).length ()
                               + from_scm<Real> (get_property (me, "padding")),
                             X_AXIS);
        }
      else
        {
          set_property (d, "X-offset", Grob::x_parent_positioning_proc);
        }
      Axis_group_interface::add_element (me, d);
    }
}

ADD_INTERFACE (Dot_column,
               R"(
Group dot objects so they form a column, and position dots so they do not clash
with staff lines.
               )",

               /* properties */
               R"(
chord-dots-limit
dots
positioning-done
direction
note-collision
padding
               )");
