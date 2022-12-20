/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "all-font-metrics.hh"

#include "string-convert.hh"
#include "international.hh"
#include "open-type-font.hh"
#include "pango-font.hh"
#include "scm-hash.hh"
#include "warn.hh"

#include <string>

#include <pango/pangofc-fontmap.h>

using std::string;

Index_to_charcode_map const *
All_font_metrics::get_index_to_charcode_map (const string &filename,
                                             int face_index, FT_Face face)
{
  string key = filename + std::to_string (face_index);
  if (filename_charcode_maps_map_.find (key)
      == filename_charcode_maps_map_.end ())
    filename_charcode_maps_map_[key] = make_index_to_charcode_map (face);

  return &filename_charcode_maps_map_[key];
}

All_font_metrics::All_font_metrics (const string &path)
{
  pango_dict_ = 0;

  otf_dict_ = 0;
  smobify_self ();
  otf_dict_ = unsmob<Scheme_hash_table> (Scheme_hash_table::make_smob ());

  pango_dict_ = unsmob<Scheme_hash_table> (Scheme_hash_table::make_smob ());
  PangoFontMap *pfm = pango_ft2_font_map_new ();

  pango_ft2_fontmap_ = PANGO_FT2_FONT_MAP (pfm);

  pango_dpi_ = PANGO_RESOLUTION;
  pango_ft2_font_map_set_resolution (pango_ft2_fontmap_, pango_dpi_,
                                     pango_dpi_);

  search_path_.parse_path (path);
}

All_font_metrics::~All_font_metrics ()
{
  g_object_unref (pango_ft2_fontmap_);
}

SCM
All_font_metrics::mark_smob () const
{
  if (pango_dict_)
    scm_gc_mark (pango_dict_->self_scm ());
  if (otf_dict_)
    return otf_dict_->self_scm ();
  return SCM_UNDEFINED;
}

Pango_font *
All_font_metrics::find_pango_font (PangoFontDescription const *description,
                                   Real output_scale)
{
  gchar *pango_fn = pango_font_description_to_filename (description);
  SCM key = ly_symbol2scm (pango_fn);

  SCM val;
  if (!pango_dict_->try_retrieve (key, &val))
    {
      debug_output ("[" + string (pango_fn), true); // start on a new line

      Pango_font *pf
        = new Pango_font (pango_ft2_fontmap_, description, output_scale);

      val = pf->self_scm ();
      pango_dict_->set (key, val);
      pf->unprotect ();

      debug_output ("]", false);

      pf->description_ = scm_cons (SCM_BOOL_F, to_scm (1.0));
    }
  g_free (pango_fn);
  return unsmob<Pango_font> (val);
}

Open_type_font *
All_font_metrics::find_otf (const string &name)
{
  SCM sname = ly_symbol2scm (name);
  SCM val;
  if (!otf_dict_->try_retrieve (sname, &val))
    {
      string file_name;

      if (file_name.empty ())
        file_name = search_path_.find (name + ".otf");
      if (file_name.empty ())
        return 0;

      debug_output ("[" + file_name, true); // start on a new line
      val = Open_type_font::make_otf (file_name);
      Open_type_font *otf = unsmob<Open_type_font> (val);

      debug_output ("]", false);

      SCM name_string = ly_string2scm (name);
      otf->description_ = scm_cons (name_string, to_scm (1.0));
      otf_dict_->set (sname, val);
      otf->unprotect ();
    }

  return unsmob<Open_type_font> (val);
}

Font_metric *
All_font_metrics::find_font (const string &name)
{
  Font_metric *f = find_otf (name);

  if (!f)
    {
      error (_f ("cannot find font: `%s'", name.c_str ()));
    }

  return f;
}

void
All_font_metrics::notify_fc_config_change ()
{
  pango_fc_font_map_config_changed (PANGO_FC_FONT_MAP (pango_ft2_fontmap_));
}
