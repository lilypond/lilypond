/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cmath>
#include <cstdio>
#include <map>
#include <set>

#include "axis-group-interface.hh"
#include "directional-element-interface.hh"
#include "dot-column.hh"
#include "dot-configuration.hh"
#include "dot-formatting-problem.hh"
#include "dots.hh"
#include "grob.hh"
#include "note-head.hh"
#include "pointer-group-interface.hh"
#include "rest.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"

using std::set;
using std::vector;

MAKE_SCHEME_CALLBACK (Dot_column, calc_positioning_done, 1);
SCM
Dot_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  /*
    Trigger note collision resolution first, since that may kill off
    dots when merging.
  */
  if (Grob *collision = unsmob<Grob> (me->get_object ("note-collision")))
    (void)collision->get_property ("positioning-done");

  me->set_property ("positioning-done", SCM_BOOL_T);

  vector<Grob *> dots = extract_grob_array (me, "dots");

  vector<Grob *> parent_stems;
  Real ss = 0;

  Grob *commonx = me;
  for (vsize i = 0; i < dots.size (); i++)
    {
      Grob *n = dots[i]->get_parent (Y_AXIS);
      commonx = n->common_refpoint (commonx, X_AXIS);

      if (Grob *stem = unsmob<Grob> (n->get_object ("stem")))
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

      if (Grob *stem = unsmob<Grob> (s->get_object ("stem")))
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
  vector_sort (dots, pure_position_less);

  SCM chord_dots_limit = me->get_property ("chord-dots-limit");
  if (scm_is_number (chord_dots_limit))
    {
      // Sort dots by stem, then check for dots above the limit for each stem
      vector<vector<Grob *>> dots_each_stem (parent_stems.size ());
      for (vsize i = 0; i < dots.size (); i++)
        if (Grob *stem
            = unsmob<Grob> (dots[i]->get_parent (Y_AXIS)->get_object ("stem")))
          for (vsize j = 0; j < parent_stems.size (); j++)
            if (stem == parent_stems[j])
              {
                dots_each_stem[j].push_back (dots[i]);
                break;
              }
      for (vsize j = 0; j < parent_stems.size (); j++)
        {
          Interval chord = Stem::head_positions (parent_stems[j]);
          int total_room
              = ((int)chord.length () + 2 + scm_to_int (chord_dots_limit)) / 2;
          int total_dots = dots_each_stem[j].size ();
          // remove excessive dots from the ends of the stem
          for (int first_dot = 0; total_dots > total_room; total_dots--)
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

      Grob *note = dots[i]->get_parent (Y_AXIS);
      if (note)
        {
          if (has_interface<Note_head> (note))
            dp.dir_ = to_dir (dp.dot_->get_property ("direction"));

          dp.x_extent_ = note->extent (commonx, X_AXIS);
        }

      int p = Staff_symbol_referencer::pure_get_rounded_position (dp.dot_);

      /* icky, since this should go via a Staff_symbol_referencer
         offset callback but adding a dot overwrites Y-offset. */
      p += (int)robust_scm2double (dp.dot_->get_property ("staff-position"),
                                   0.0);
      dp.pos_ = p;

      cfg.remove_collision (p);
      cfg[p] = dp;
      if (Staff_symbol_referencer::on_line (dp.dot_, p)
          && !scm_is_eq (dp.dot_->get_property ("style"),
                         ly_symbol2scm ("kievan")))
        cfg.remove_collision (p);
    }

  for (Dot_configuration::const_iterator i (cfg.begin ()); i != cfg.end (); i++)
    {
      /*
        Junkme?
       */
      Staff_symbol_referencer::pure_set_position (i->second.dot_, i->first);
    }

  me->translate_axis (
      cfg.x_offset () - me->relative_coordinate (commonx, X_AXIS), X_AXIS);
  return SCM_BOOL_T;
}

void
Dot_column::add_head (Grob *me, Grob *head)
{
  Grob *d = unsmob<Grob> (head->get_object ("dot"));
  if (d)
    {
      Side_position_interface::add_support (me, head);

      Pointer_group_interface::add_grob (me, ly_symbol2scm ("dots"), d);
      d->set_property ("Y-offset", Grob::x_parent_positioning_proc);
      // Dot formatting requests the Y-offset, which for rests may
      // trigger post-linebreak callbacks.  On the other hand, we need the
      // correct X-offset of the dots for horizontal collision avoidance.
      // The translation here is undone in calc_positioning_done, where we
      // do the X-offset properly.
      if (has_interface<Rest> (head))
        d->translate_axis (head->extent (head, X_AXIS).length (), X_AXIS);
      else
        d->set_property ("X-offset", Grob::x_parent_positioning_proc);
      Axis_group_interface::add_element (me, d);
    }
}

ADD_INTERFACE (Dot_column,
               "Group dot objects so they form a column, and position"
               " dots so they do not clash with staff lines.",

               /* properties */
               "chord-dots-limit "
               "dots "
               "positioning-done "
               "direction "
               "note-collision ");
