/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "dimensions.hh"
#include "all-font-metrics.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "warn.hh"
#include "pango-font.hh"
#include "lily-imports.hh"

Font_metric *
get_font_by_design_size (Output_def *layout, Real requested, SCM font_vector)
{
  size_t n = scm_c_vector_length (font_vector);
  Real size = 1e6;
  Real last_size = -1e6;
  size_t i = 0;

  SCM pango_description_string = SCM_EOL;
  SCM last_pango_description_string = SCM_EOL;
  for (; i < n; i++)
    {
      SCM entry = scm_c_vector_ref (font_vector, i);

      if (from_scm<bool> (scm_promise_p (entry)))
        {
          Font_metric *fm = unsmob<Font_metric> (scm_force (entry));
          size = fm->design_size ();
        }
      else if (scm_is_pair (entry) && scm_is_number (scm_car (entry))
               && scm_is_string (scm_cdr (entry)))
        {
          size = from_scm<double> (scm_car (entry));
          pango_description_string = scm_cdr (entry);
        }
      if (size > requested)
        break;
      last_size = size;
      last_pango_description_string = pango_description_string;
    }

  if (i == n)
    i = n - 1;
  else if (i > 0)
    {
      if ((requested / last_size) < (size / requested))
        {
          i--;
          size = last_size;
          pango_description_string = last_pango_description_string;
        }
    }

  Font_metric *fm = 0;
  if (scm_is_string (pango_description_string))
    {
      return find_pango_font (layout, pango_description_string,
                              requested / size);
    }
  else
    fm = unsmob<Font_metric> (scm_force (scm_c_vector_ref (font_vector, i)));

  return find_scaled_font (layout, fm, requested / size);
}

Font_metric *
get_font_by_mag_step (Output_def *layout, Real requested_step, SCM font_vector,
                      Real default_size)
{
  return get_font_by_design_size (
    layout, default_size * pow (2.0, requested_step / 6.0), font_vector);
}

SCM
properties_to_font_size_family (SCM fonts, SCM alist_chain)
{
  return Lily::lookup_font (fonts, alist_chain);
}

Font_metric *
select_encoded_font (Output_def *layout, SCM chain)
{
  SCM name
    = ly_chain_assoc_get (ly_symbol2scm ("font-name"), chain, SCM_BOOL_F);

  if (!scm_is_string (name))
    {
      SCM fonts = layout->lookup_variable (ly_symbol2scm ("fonts"));
      name = properties_to_font_size_family (fonts, chain);
    }

  if (scm_is_string (name))
    return select_pango_font (layout, chain);
  else if (scm_is_true (scm_instance_p (name)))
    {
      SCM base_size = scm_slot_ref (name, ly_symbol2scm ("default-size"));
      SCM vec = scm_slot_ref (name, ly_symbol2scm ("size-vector"));

      Real req = from_scm<double> (
        ly_chain_assoc_get (ly_symbol2scm ("font-size"), chain, SCM_BOOL_F),
        0.0);

      return get_font_by_mag_step (layout, req, vec,
                                   from_scm<double> (base_size));
    }

  assert (0);
  return 0;
}

Font_metric *
select_font (Output_def *layout, SCM chain)
{
  return select_encoded_font (layout, chain);
}
