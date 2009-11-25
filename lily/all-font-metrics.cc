/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "main.hh"
#include "open-type-font.hh"
#include "pango-font.hh"
#include "scm-hash.hh"
#include "warn.hh"


Index_to_charcode_map const *
All_font_metrics::get_index_to_charcode_map (string filename,
					     int face_index,
					     FT_Face face)
{
  string key = filename + String_convert::int_string (face_index);
  if (filename_charcode_maps_map_.find (key)
      == filename_charcode_maps_map_.end ())
    filename_charcode_maps_map_[key] = make_index_to_charcode_map (face);

  return &filename_charcode_maps_map_[key];
}


All_font_metrics::All_font_metrics (string path)
{
  otf_dict_ = new Scheme_hash_table;

#if HAVE_PANGO_FT2
  PangoFontMap *pfm = pango_ft2_font_map_new ();

  pango_ft2_fontmap_
    = G_TYPE_CHECK_INSTANCE_CAST (pfm,
				  PANGO_TYPE_FT2_FONT_MAP,
				  PangoFT2FontMap);
  pango_dpi_ = 1200;
  pango_ft2_font_map_set_resolution (pango_ft2_fontmap_,
				     pango_dpi_, pango_dpi_);

  pango_dict_ = new Scheme_hash_table;
#endif

  search_path_.parse_path (path);
}

All_font_metrics::~All_font_metrics ()
{
  otf_dict_->unprotect ();

#if HAVE_PANGO_FT2
  pango_dict_->unprotect ();
  g_object_unref (pango_ft2_fontmap_);
#endif
}

All_font_metrics::All_font_metrics (All_font_metrics const &)
{
}

#if HAVE_PANGO_FT2

Pango_font *
All_font_metrics::find_pango_font (PangoFontDescription const *description,
				   Real output_scale
				   )
{
  gchar *pango_fn = pango_font_description_to_filename (description);
  SCM key = ly_symbol2scm (pango_fn);

  SCM val;
  if (!pango_dict_->try_retrieve (key, &val))
    {
      if (be_verbose_global)
	progress_indication ("\n[" + string (pango_fn));

      Pango_font *pf = new Pango_font (pango_ft2_fontmap_,
				       description,
				       output_scale
				       );
      
      val = pf->self_scm ();
      pango_dict_->set (key, val);
      pf->unprotect ();

      if (be_verbose_global)
	progress_indication ("]");

      pf->description_ = scm_cons (SCM_BOOL_F,
				   scm_from_double (1.0));
    }
  g_free (pango_fn);
  return dynamic_cast<Pango_font *> (unsmob_metrics (val));
}

#endif


Open_type_font *
All_font_metrics::find_otf (string name)
{
  SCM sname = ly_symbol2scm (name.c_str ());
  SCM val;
  if (!otf_dict_->try_retrieve (sname, &val))
    {
      string file_name;

      if (file_name.empty ())
	file_name = search_path_.find (name + ".otf");
      if (file_name.empty ())
	return 0;

      if (be_verbose_global)
	progress_indication ("\n[" + file_name);

      val = Open_type_font::make_otf (file_name);

      if (be_verbose_global)
	progress_indication ("]");

      unsmob_metrics (val)->file_name_ = file_name;
      SCM name_string = ly_string2scm (name);
      unsmob_metrics (val)->description_ = scm_cons (name_string,
						     scm_from_double (1.0));
      otf_dict_->set (sname, val);
      unsmob_metrics (val)->unprotect ();
    }

  return dynamic_cast<Open_type_font *> (unsmob_metrics (val));
}

Font_metric *
All_font_metrics::find_font (string name)
{
  Font_metric *f = find_otf (name);

  if (!f)
    {
      error (_f ("cannot find font: `%s'", name.c_str ()));
    }

  return f;
}

All_font_metrics *all_fonts_global;
