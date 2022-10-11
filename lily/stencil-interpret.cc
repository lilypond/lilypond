/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "stencil-interpret.hh"
#include "stencil.hh"

void
interpret_stencil_expression (SCM expr, Stencil_sink *sink, Offset o)
{
  while (1)
    {
      if (!scm_is_pair (expr))
        return;

      SCM head = scm_car (expr);

      if (scm_is_eq (head, ly_symbol2scm ("delay-stencil-evaluation")))
        {
          interpret_stencil_expression (scm_force (scm_cadr (expr)), sink, o);
          return;
        }
      if (scm_is_eq (head, ly_symbol2scm ("footnote")))
        return;
      if (scm_is_eq (head, ly_symbol2scm ("translate-stencil")))
        {
          o += from_scm<Offset> (scm_cadr (expr));
          expr = scm_caddr (expr);
        }
      else if (scm_is_eq (head, ly_symbol2scm ("combine-stencil")))
        {

          for (SCM x = scm_cdr (expr); scm_is_pair (x); x = scm_cdr (x))
            interpret_stencil_expression (scm_car (x), sink, o);
          return;
        }
      else if (scm_is_eq (head, ly_symbol2scm ("grob-cause")))
        {
          SCM grob = scm_cadr (expr);
          sink->output (ly_list (head, to_scm (o), grob));
          interpret_stencil_expression (scm_caddr (expr), sink, o);
          sink->output (ly_list (ly_symbol2scm ("no-origin")));
          return;
        }
      else if (scm_is_eq (head, ly_symbol2scm ("color")))
        {
          SCM color = scm_cadr (expr);
          SCM r = scm_car (color);
          SCM g = scm_cadr (color);
          SCM b = scm_caddr (color);
          if (from_scm<int> (scm_length (color)) == 4)
            {
              SCM a = scm_cadddr (color);
              sink->output (ly_list (ly_symbol2scm ("setcolor"), r, g, b, a));
            }
          else
            sink->output (ly_list (ly_symbol2scm ("setcolor"), r, g, b));
          interpret_stencil_expression (scm_caddr (expr), sink, o);
          sink->output (ly_list (ly_symbol2scm ("resetcolor")));

          return;
        }
      else if (scm_is_eq (head, ly_symbol2scm ("output-attributes")))
        {
          SCM attributes = scm_cadr (expr);

          sink->output (
            ly_list (ly_symbol2scm ("start-group-node"), attributes));
          interpret_stencil_expression (scm_caddr (expr), sink, o);
          sink->output (ly_list (ly_symbol2scm ("end-group-node")));

          return;
        }
      else if (scm_is_eq (head, ly_symbol2scm ("rotate-stencil")))
        {
          SCM args = scm_cadr (expr);
          SCM angle = scm_car (args);
          Offset tmp = o + from_scm (scm_cadr (args), Offset (0.0, 0.0));

          SCM offset = to_scm (tmp);
          SCM x = scm_car (offset);
          SCM y = scm_cdr (offset);

          sink->output (ly_list (ly_symbol2scm ("setrotation"), angle, x, y));
          interpret_stencil_expression (scm_caddr (expr), sink, o);
          sink->output (ly_list (ly_symbol2scm ("resetrotation"), angle, x, y));

          return;
        }
      else if (scm_is_eq (head, ly_symbol2scm ("scale-stencil")))
        {
          SCM args = scm_cadr (expr);
          SCM x_scale = scm_car (args);
          SCM y_scale = scm_cadr (args);
          Offset unscaled = o.scale (Offset (1 / from_scm<double> (x_scale),
                                             1 / from_scm<double> (y_scale)));

          sink->output (ly_list (ly_symbol2scm ("setscale"), x_scale, y_scale));
          interpret_stencil_expression (scm_caddr (expr), sink, unscaled);
          sink->output (ly_list (ly_symbol2scm ("resetscale")));

          return;
        }
      else if (scm_is_eq (head, ly_symbol2scm ("with-outline")))
        {
          expr = scm_caddr (expr);
        }
      else
        {
          sink->output (ly_list (ly_symbol2scm ("settranslation"),
                                 to_scm (o[X_AXIS]), to_scm (o[Y_AXIS])));
          SCM result = sink->output (expr);
          sink->output (ly_list (ly_symbol2scm ("resettranslation")));

          if (scm_is_false (result) && scm_is_pair (expr)
              && scm_is_eq (scm_car (expr), ly_symbol2scm ("utf-8-string")))
            {
              expr = scm_list_ref (expr, SCM_I_MAKINUM (3));
              continue;
            }

          return;
        }
    }
}
