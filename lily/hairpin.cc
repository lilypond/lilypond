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

#include "hairpin.hh"

#include "axis-group-interface.hh"
#include "dimensions.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "line-interface.hh"
#include "note-column.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "system.hh"
#include "text-interface.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Hairpin, pure_height, 3);
SCM
Hairpin::pure_height (SCM smob, SCM, SCM)
{
  Grob *me = unsmob<Grob> (smob);
  Real height = robust_scm2double (me->get_property ("height"), 0.0)
                * Staff_symbol_referencer::staff_space (me);

  Real thickness = robust_scm2double (me->get_property ("thickness"), 1)
                   * Staff_symbol_referencer::line_thickness (me);

  height += thickness / 2;
  return ly_interval2scm (Interval (-height, height));
}

MAKE_SCHEME_CALLBACK (Hairpin, broken_bound_padding, 1);
SCM
Hairpin::broken_bound_padding (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Item *r_bound = me->get_bound (RIGHT);
  if (r_bound->break_status_dir () != -1)
    {
      me->warning (
          _ ("Asking for broken bound padding at a non-broken bound."));
      return scm_from_double (0.0);
    }

  System *sys = dynamic_cast<System *> (me->get_system ());
  Direction dir = get_grob_direction (me->get_parent (Y_AXIS));
  if (!dir)
    return scm_from_double (0.0);

  Grob *my_vertical_axis_group = Grob::get_vertical_axis_group (me);
  Drul_array<Grob *> vertical_axis_groups;
  for (DOWN_and_UP (d))
    vertical_axis_groups[d]
        = d == dir ? sys->get_neighboring_staff (
              d, my_vertical_axis_group,
              Interval_t<int> (me->spanned_rank_interval ()))
                   : my_vertical_axis_group;

  if (!vertical_axis_groups[dir])
    return scm_from_double (0.0);

  Drul_array<Grob *> span_bars (0, 0);
  for (DOWN_and_UP (d))
    {
      extract_grob_set (vertical_axis_groups[d], "elements", elts);
      for (vsize i = elts.size (); i--;)
        if (elts[i]->internal_has_interface (
                ly_symbol2scm ("bar-line-interface"))
            && dynamic_cast<Item *> (elts[i])->break_status_dir () == -1)
          {
            SCM hsb = elts[i]->get_property ("has-span-bar");
            if (!scm_is_pair (hsb))
              break;

            span_bars[d] = unsmob<Grob> ((d == UP ? scm_car : scm_cdr) (hsb));
            break;
          }

      if (!span_bars[d])
        return scm_from_double (0.0);
    }

  if (span_bars[DOWN] != span_bars[UP])
    return scm_from_double (0.0);

  return scm_from_double (
      robust_scm2double (me->get_property ("bound-padding"), 0.5) / 2.0);
}

MAKE_SCHEME_CALLBACK (Hairpin, print, 1);
SCM
Hairpin::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

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
  for (LEFT_and_RIGHT (d))
    {
      bounds[d] = me->get_bound (d);
      broken[d] = bounds[d]->break_status_dir () != CENTER;
    }

  if (broken[RIGHT])
    {
      Spanner *next = me->broken_neighbor (RIGHT);
      // Hairpin-parts suicide in after-line-breaking if they need not be drawn
      if (next)
        {
          (void)next->get_property ("after-line-breaking");
          broken[RIGHT] = next->is_live ();
        }
      else
        broken[RIGHT] = false;
    }

  Grob *common = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);
  Drul_array<Real> x_points;

  /*
    Use the height and thickness of the hairpin when making a circled tip
  */
  bool circled_tip = ly_scm2bool (me->get_property ("circled-tip"));
  Real height = robust_scm2double (me->get_property ("height"), 0.2)
                * Staff_symbol_referencer::staff_space (me);
  /*
    FIXME: 0.525 is still just a guess...
  */
  Real rad = height * 0.525;
  Real thick = 1.0;
  if (circled_tip)
    thick = robust_scm2double (me->get_property ("thickness"), 1.0)
            * Staff_symbol_referencer::line_thickness (me);
  Drul_array<Real> shorten = robust_scm2interval (
      me->get_property ("shorten-pair"), Interval (0, 0));

  for (LEFT_and_RIGHT (d))
    {
      Item *b = bounds[d];
      Interval e
          = Axis_group_interface::generic_bound_extent (b, common, X_AXIS);

      x_points[d] = b->relative_coordinate (common, X_AXIS);
      if (broken[d])
        {
          if (d == LEFT)
            x_points[d] = e[-d];
          else
            {
              Real broken_bound_padding = robust_scm2double (
                  me->get_property ("broken-bound-padding"), 0.0);
              extract_grob_set (me, "concurrent-hairpins", chp);
              for (vsize i = 0; i < chp.size (); i++)
                {
                  Spanner *span_elt = dynamic_cast<Spanner *> (chp[i]);
                  if (span_elt->get_bound (RIGHT)->break_status_dir () == LEFT)
                    broken_bound_padding = std::max (
                        broken_bound_padding,
                        robust_scm2double (
                            span_elt->get_property ("broken-bound-padding"),
                            0.0));
                }
              x_points[d] -= d * broken_bound_padding;
            }
        }
      else
        {
          if (has_interface<Text_interface> (b))
            {
              if (!e.is_empty ())
                x_points[d] = e[-d] - d * padding;
            }
          else
            {
              bool neighbor_found = false;
              Spanner *adjacent = NULL;
              extract_grob_set (me, "adjacent-spanners", neighbors);
              for (vsize i = 0; i < neighbors.size (); i++)
                {
                  /*
                    FIXME: this will fuck up in case of polyphonic
                    notes in other voices. Need to look at note-columns
                    in the current staff/voice.
                  */
                  adjacent = dynamic_cast<Spanner *> (neighbors[i]);
                  if (adjacent
                      && (adjacent->get_bound (-d)->get_column ()
                          == b->get_column ()))
                    {
                      neighbor_found = true;
                      break;
                    }
                }

              if (neighbor_found)
                {
                  if (has_interface<Hairpin> (adjacent))
                    {
                      /*
                        Handle back-to-back hairpins with a circle in the middle
                      */
                      if (circled_tip && (grow_dir != d))
                        {
                          x_points[d] = e.center () + d * (rad - thick / 2.0);
                          shorten[d] = 0.0;
                        }
                      /*
                        If we're hung on a paper column, that means we're not
                        adjacent to a text-dynamic, and we may move closer. We
                        make the padding a little smaller, here.
                      */
                      else
                        x_points[d] = e.center () - d * padding / 3;
                    }
                  // Our neighbor is a dynamic text spanner.
                  // If we end on the text, pad as for text dynamics
                  else if (d == RIGHT)
                    x_points[d] = e[-d] - d * padding;
                }
              else
                {
                  if (d == RIGHT // end at the left edge of a rest
                      && has_interface<Note_column> (b)
                      && Note_column::has_rests (b))
                    x_points[d] = e[-d];
                  else
                    x_points[d] = e[d];

                  if (Item::is_non_musical (b))
                    x_points[d] -= d * padding;
                }
            }
        }

      x_points[d] -= shorten[d] * d;
    }

  Real width = x_points[RIGHT] - x_points[LEFT];

  if (width < 0)
    {
      me->warning (
          _ ((grow_dir < 0) ? "decrescendo too small" : "crescendo too small"));
      width = 0;
    }

  bool continued = broken[Direction (-grow_dir)];
  bool continuing = broken[Direction (grow_dir)];

  Real starth = 0;
  Real endh = 0;
  if (grow_dir < 0)
    {
      starth = continuing ? 2 * height / 3 : height;
      endh = continued ? height / 3 : 0.0;
    }
  else
    {
      starth = continued ? height / 3 : 0.0;
      endh = continuing ? 2 * height / 3 : height;
    }

  /*
    should do relative to staff-symbol staff-space?
  */
  Stencil mol;
  Real x = 0.0;

  /*
    Compensate for size of circle
  */
  Direction tip_dir = -grow_dir;
  if (circled_tip && !broken[tip_dir])
    {
      if (grow_dir > 0)
        x = rad * 2.0;
      else if (grow_dir < 0)
        width -= rad * 2.0;
    }
  mol = Line_interface::line (me, Offset (x, starth), Offset (width, endh));
  mol.add_stencil (
      Line_interface::line (me, Offset (x, -starth), Offset (width, -endh)));

  /*
    Support al/del niente notation by putting a circle at the
    tip of the (de)crescendo.
  */
  if (circled_tip)
    {
      Box extent (Interval (-rad, rad), Interval (-rad, rad));

      /* Hmmm, perhaps we should have a Lookup::circle () method? */
      Stencil circle (extent, scm_list_4 (ly_symbol2scm ("circle"),
                                          scm_from_double (rad),
                                          scm_from_double (thick), SCM_BOOL_F));

      /*
        don't add another circle if the hairpin is broken
      */
      if (!broken[tip_dir])
        mol.add_at_edge (X_AXIS, tip_dir, Stencil (circle), 0);
    }

  mol.translate_axis (x_points[LEFT]
                          - bounds[LEFT]->relative_coordinate (common, X_AXIS),
                      X_AXIS);
  return mol.smobbed_copy ();
}

ADD_INTERFACE (Hairpin, "A hairpin crescendo or decrescendo.",

               /* properties */
               "adjacent-spanners "
               "circled-tip "
               "concurrent-hairpins "
               "broken-bound-padding "
               "bound-padding "
               "grow-direction "
               "height "
               "shorten-pair ");
