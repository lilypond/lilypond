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

#include "rest-collision.hh"

#include "directional-element-interface.hh"
#include "duration.hh"
#include "international.hh"
#include "item.hh"
#include "note-column.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "rest.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "grob.hh"
#include "unpure-pure-container.hh"
#include "warn.hh"
#include "lily-imports.hh"

#include <algorithm>
#include <cmath> // ceil.
#include <vector>

using std::vector;

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (
  Rest_collision, force_shift_callback_rest,
  "ly:rest-collision::force-shift-callback-rest", 2, 1, "");
SCM
Rest_collision::force_shift_callback_rest (SCM rest, SCM offset)
{
  auto *const rest_grob = LY_ASSERT_SMOB (Grob, rest, 1);
  Grob *parent = rest_grob->get_x_parent ();

  /*
    translate REST; we need the result of this translation later on,
    while the offset probably still is 0/calculation-in-progress.
   */
  if (scm_is_number (offset))
    rest_grob->translate_axis (from_scm<double> (offset), Y_AXIS);

  if (has_interface<Note_column> (parent) && Note_column::has_rests (parent))
    {
      Grob *collision = unsmob<Grob> (get_object (parent, "rest-collision"));

      if (collision)
        (void) get_property (collision, "positioning-done");
    }

  return to_scm (0.0);
}

void
Rest_collision::add_column (Grob *me, Grob *p)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), p);

  set_object (p, "rest-collision", me->self_scm ());

  Grob *rest = unsmob<Grob> (get_object (p, "rest"));
  if (rest)
    {
      chain_offset_callback (rest,
                             Unpure_pure_container::make_smob (
                               Rest_collision::force_shift_callback_rest_proc,
                               Lily::pure_chain_offset_callback),
                             Y_AXIS);
    }
}

static bool
rest_shift_less (Grob *const &r1, Grob *const &r2)
{
  Grob *col1 = r1->get_x_parent ();
  Grob *col2 = r2->get_x_parent ();
  return Note_column::shift_less (col1, col2);
}

/*
  TODO: look at horizontal-shift to determine ordering between rests
  for more than two voices.
*/
MAKE_SCHEME_CALLBACK (Rest_collision, calc_positioning_done,
                      "ly:rest-collision::calc-positioning-done", 1);
SCM
Rest_collision::calc_positioning_done (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  set_property (me, "positioning-done", SCM_BOOL_T);

  extract_grob_set (me, "elements", elts);

  vector<Grob *> rests;
  vector<Grob *> notes;

  for (vsize i = 0; i < elts.size (); i++)
    {
      Grob *e = elts[i];
      if (has_interface<Note_column> (e))
        {
          if (unsmob<Grob> (get_object (e, "rest")))
            rests.push_back (e);
          else
            notes.push_back (e);
        }
    }

  /*
    handle rest-rest and rest-note collisions

    [todo]
    * decide not to print rest if too crowded?
    */

  /*
    no partners to collide with
  */
  if (rests.size () + notes.size () < 2)
    return SCM_BOOL_T;

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  /*
    only rests
  */
  if (!notes.size ())
    {

      /*
        This is incomplete: in case of an uneven number of rests, the
        center one should be centered on the staff.
      */
      Drul_array<vector<Grob *>> ordered_rests;
      for (vsize i = 0; i < rests.size (); i++)
        {
          Grob *r = Note_column::get_rest (rests[i]);

          Direction d = get_grob_direction (r);
          if (d)
            ordered_rests[d].push_back (r);
          else
            rests[0]->warning (
              _ ("cannot resolve rest collision: rest direction not set"));
        }

      for (const auto d : {LEFT, RIGHT})
        std::sort (ordered_rests[d].begin (), ordered_rests[d].end (),
                   rest_shift_less);

      for (const auto d : {LEFT, RIGHT})
        {
          if (ordered_rests[d].size () < 1)
            {
              if (ordered_rests[-d].size () > 1)
                ordered_rests[-d][0]->warning (_ ("too many colliding rests"));

              return SCM_BOOL_T;
            }
        }

      Grob *common = common_refpoint_of_array (ordered_rests[DOWN], me, Y_AXIS);
      common = common_refpoint_of_array (ordered_rests[UP], common, Y_AXIS);

      Real diff = (ordered_rests[DOWN].back ()->extent (common, Y_AXIS)[UP]
                   - ordered_rests[UP].back ()->extent (common, Y_AXIS)[DOWN])
                  / staff_space;

      if (diff > 0)
        {
          const auto amount_down = static_cast<int> (ceil (diff / 2));
          diff -= amount_down;
          Rest::translate (ordered_rests[DOWN].back (), -2 * amount_down);
          if (diff > 0)
            Rest::translate (ordered_rests[UP].back (), 2 * int (ceil (diff)));
        }

      for (const auto d : {LEFT, RIGHT})
        {
          for (vsize i = ordered_rests[d].size () - 1; i-- > 0;)
            {
              Real last_y = ordered_rests[d][i + 1]->extent (common, Y_AXIS)[d];
              Real y = ordered_rests[d][i]->extent (common, Y_AXIS)[-d];

              Real diff = d * ((last_y - y) / staff_space);
              if (diff > 0)
                {
                  const auto amount = static_cast<int> (ceil (diff)) * 2;
                  Rest::translate (ordered_rests[d][i], d * amount);
                }
            }
        }
    }
  else
    {
      /*
        Rests and notes.
      */
      // Count how many rests we move
      Drul_array<int> rcount;

      for (vsize i = 0; i < rests.size (); i++)
        {
          Grob *rcol = rests[i];
          Grob *rest = Note_column::get_rest (rcol);

          Direction dir = get_grob_direction (rest);
          if (!dir)
            dir = Note_column::dir (rcol);

          // Do not compute a translation for pre-positioned rests,
          //  nor count them for the "too many colliding rests" warning
          if (scm_is_number (get_property (rest, "staff-position")))
            continue;

          Grob *common = common_refpoint_of_array (notes, rcol, Y_AXIS);
          Interval restdim = rest->extent (common, Y_AXIS);
          if (restdim.is_empty ())
            continue;

          Real staff_space = Staff_symbol_referencer::staff_space (rcol);
          Real minimum_dist
            = from_scm<double> (get_property (me, "minimum-distance"), 1.0)
              * staff_space;

          Interval notedim;
          for (vsize i = 0; i < notes.size (); i++)
            {
              if (
                Note_column::dir (notes[i]) == -dir
                // If the note has already happened (but it has a long
                // duration, so there is a collision), don't look at the stem.
                // If we do, the rest gets shifted down a lot and it looks bad.
                || dynamic_cast<Item *> (notes[i])->get_column ()
                     != dynamic_cast<Item *> (rest)->get_column ())
                {
                  /* try not to look at the stem, as looking at a beamed
                     note may trigger beam positioning prematurely.

                     This happens with dotted rests, which need Y
                     positioning to compute X-positioning.
                  */
                  Grob *head = Note_column::first_head (notes[i]);
                  if (head)
                    notedim.unite (head->extent (common, Y_AXIS));
                  else
                    programming_error ("Note_column without first_head()");
                }
              else
                notedim.unite (notes[i]->extent (common, Y_AXIS));
            }

          Real y = dir
                   * std::max (0.0, -dir * restdim[-dir] + dir * notedim[dir]
                                      + minimum_dist);

          // move discretely by half spaces.
          int discrete_y = dir * int (ceil (y / (0.5 * dir * staff_space)));

          Interval staff_span = Staff_symbol_referencer::staff_span (rest);
          staff_span.widen (1);
          // move by whole spaces inside the staff.
          if (staff_span.contains (Staff_symbol_referencer::get_position (rest)
                                   + discrete_y))
            {
              discrete_y = dir * int (ceil (dir * discrete_y / 2.0) * 2.0);
            }

          Rest::translate (rest, discrete_y);
          if (rcount[dir]++)
            rest->warning (_ ("too many colliding rests"));
        }
    }
  return SCM_BOOL_T;
}

ADD_INTERFACE (Rest_collision,
               R"(
Move ordinary rests (not multi-measure nor pitched rests) to avoid conflicts.
               )",

               /* properties */
               R"(
minimum-distance
positioning-done
elements
               )");
