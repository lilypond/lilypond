/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "config.hh"

#include "dimensions.hh"
#include "libc-extension.hh"
#include "output-def.hh"
#include "modified-font-metric.hh"
#include "pango-font.hh"
#include "all-font-metrics.hh"
#include "lily-imports.hh"

#include <cmath>
#include <string>

Real
output_scale (Output_def *od)
{
  return from_scm<double> (
    od->lookup_variable (ly_symbol2scm ("output-scale")));
}

SCM
get_font_table (Output_def *def)
{
  SCM font_table = def->lookup_variable (ly_symbol2scm ("scaled-fonts"));
  if (!from_scm<bool> (scm_hash_table_p (font_table)))
    {
      font_table = scm_c_make_hash_table (11);
      def->set_variable (ly_symbol2scm ("scaled-fonts"), font_table);
    }
  return font_table;
}

SCM
get_pango_font_table (Output_def *def)
{
  SCM font_table = def->lookup_variable (ly_symbol2scm ("pango-fonts"));
  if (!from_scm<bool> (scm_hash_table_p (font_table)))
    {
      font_table = scm_c_make_hash_table (11);
      def->set_variable (ly_symbol2scm ("pango-fonts"), font_table);
    }
  return font_table;
}

/* TODO: should add nesting for Output_def here too. */
Font_metric *
find_scaled_font (Output_def *mod, Font_metric *f, Real m)
{
  if (mod->parent_)
    return find_scaled_font (mod->parent_, f, m);

  Real lookup_mag = m / output_scale (mod);

  SCM font_table = get_font_table (mod);
  SCM sizes = scm_hashq_ref (font_table, f->self_scm (), SCM_EOL);
  SCM handle = ly_assoc (to_scm (lookup_mag), sizes);
  if (scm_is_pair (handle))
    return unsmob<Font_metric> (scm_cdr (handle));

  SCM val = Modified_font_metric::make_scaled_font_metric (f, lookup_mag);

  sizes = scm_acons (to_scm (lookup_mag), val, sizes);
  unsmob<Font_metric> (val)->unprotect ();
  scm_hashq_set_x (font_table, f->self_scm (), sizes);
  return unsmob<Font_metric> (val);
}

Font_metric *
find_pango_font (Output_def *layout, SCM descr, Real factor)
{
  if (layout->parent_)
    return find_pango_font (layout->parent_, descr, factor);

  SCM table = get_pango_font_table (layout);
  SCM sizes = scm_hash_ref (table, descr, SCM_EOL);
  SCM size_key = to_scm (factor);
  SCM handle = ly_assoc (size_key, sizes);
  if (scm_is_pair (handle))
    return unsmob<Font_metric> (scm_cdr (handle));

  std::string font_descr = ly_scm2string (descr);
  PangoFontDescription *description
    = pango_font_description_from_string (font_descr.c_str ());

  double font_description_size
    = round_halfway_up (factor * pango_font_description_get_size (description));
  pango_font_description_set_size (description, gint (font_description_size));

  Font_metric *fm
    = all_fonts_global->find_pango_font (description, output_scale (layout));

  pango_font_description_free (description);

  sizes = scm_acons (size_key, fm->self_scm (), sizes);
  scm_hash_set_x (table, descr, sizes);

  return fm;
}

/* TODO: this is a nasty interface. During formatting,
   the Output_def should be scaled to the output_scale_
   specified in the toplevel Output_def.  */
Output_def *
scale_output_def (Output_def *o, Real amount)
{
  SCM new_pap = Lily::scale_layout (o->self_scm (), to_scm (amount));

  o = unsmob<Output_def> (new_pap);
  o->protect ();
  return o;
}
