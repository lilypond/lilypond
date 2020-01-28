/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2019--2020 David Nalesnik <david.nalesnik@gmail.com>

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

#include "measure-spanner.hh"

#include "bracket.hh"
#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "text-interface.hh"

MAKE_SCHEME_CALLBACK (Measure_spanner, calc_connect_to_neighbors, 1);
SCM
Measure_spanner::calc_connect_to_neighbors (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);

  Spanner *orig_spanner = me->original ();
  if (!orig_spanner)
    return SCM_EOL;

  Drul_array<Item *> bounds (me->get_bound (LEFT), me->get_bound (RIGHT));
  Drul_array<bool> connect_to_other (false, false);

  for (LEFT_and_RIGHT (d))
    {
      Direction break_dir = bounds[d]->break_status_dir ();
      vsize neighbor_idx = me->get_break_index () - break_dir;

      connect_to_other[d]
          = (break_dir && neighbor_idx < orig_spanner->broken_intos_.size ()
             && orig_spanner->broken_intos_[neighbor_idx]->is_live ());
    }

  if (connect_to_other[LEFT] || connect_to_other[RIGHT])
    return scm_cons (scm_from_bool (connect_to_other[LEFT]),
                     scm_from_bool (connect_to_other[RIGHT]));

  return SCM_EOL;
}

MAKE_SCHEME_CALLBACK (Measure_spanner, print, 1);
SCM
Measure_spanner::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  Stencil mol;
  Stencil brack;
  SCM txt = me->get_property ("text");

  SCM visible = me->get_property ("bracket-visibility");

  Drul_array<Item *> bounds (me->get_bound (LEFT), me->get_bound (RIGHT));

  /* should store note columns in engraver? */
  Grob *common_x = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);
  Drul_array<Real> x_points;

  SCM sp = me->get_property ("spacing-pair");
  SCM align_syms;

  for (LEFT_and_RIGHT (d))
    {
      align_syms = (scm_is_pair (sp) ? index_get_cell (sp, d)
                                     : ly_symbol2scm ("staff-bar"));
      x_points[d] = Paper_column::break_align_width (bounds[d], align_syms)[-d];
    }

  Stencil bracket_text;
  Interval gap_iv;

  if (Text_interface::is_markup (txt))
    {
      Output_def *pap = me->layout ();
      SCM properties = Font_interface::text_font_alist_chain (me);
      SCM t = Text_interface::interpret_markup (pap->self_scm (), properties,
                                                txt);
      bracket_text = *unsmob<Stencil> (t);
      bracket_text.align_to (X_AXIS, CENTER);
      Interval stil_Y_ext = bracket_text.extent (Y_AXIS);
      bracket_text.translate_axis ((x_points[RIGHT] - x_points[LEFT]) / 2.0,
                                   X_AXIS);
      bracket_text.translate_axis (-stil_Y_ext[UP] / 2.0, Y_AXIS);
      Real gap = bracket_text.extent (X_AXIS).length ();
      gap_iv = Interval (-0.5, 0.5) * gap;
      gap_iv.widen (0.6);
    }

  if (ly_scm2bool (visible))
    brack = Bracket::make_axis_constrained_bracket (
        me, x_points[RIGHT] - x_points[LEFT], X_AXIS, get_grob_direction (me),
        gap_iv);

  if (!bracket_text.is_empty ())
    brack.add_stencil (bracket_text);

  mol.add_stencil (brack);

  Real me_coord = me->relative_coordinate (common_x, X_AXIS);

  mol.translate_axis (x_points[LEFT] - me_coord, X_AXIS);

  return mol.smobbed_copy ();
}

ADD_INTERFACE (Measure_spanner, "A bracket aligned to a measure or measures.",

               /* properties */
               "bracket-flare "
               "bracket-visibility "
               "connect-to-neighbor "
               "direction "
               "edge-height "
               "padding "
               "shorten-pair "
               "spacing-pair "
               "staff-padding "
               "thickness ");
