/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef ALL_FONTS_HH
#define ALL_FONTS_HH

#include "file-path.hh"
#include "font-config.hh"
#include "font-metric.hh"
#include "config.hh"

#include <fontconfig/fontconfig.h>
#include <pango/pango.h>
#include <pango/pangoft2.h>

/*
   Interface to all fonts (both system fonts and fonts loaded
   via Pango).
*/

class All_font_metrics : public Smob<All_font_metrics>
{
  Scheme_hash_table *otf_dict_;
  File_path search_path_;

  PangoFT2FontMap *pango_ft2_fontmap_;
  static PangoFT2FontMap *emmentaler_pango_ft2_fontmap_;
  Scheme_hash_table *pango_dict_;
  int pango_dpi_;

  std::map<std::string, Index_to_charcode_map> filename_charcode_maps_map_;

  unique_fcconfig_ptr font_config_ = nullptr;
  bool font_config_has_app_fonts_ = false;
  static unique_fcconfig_ptr emmentaler_font_config_;

  void font_config_changed ();

public:
  SCM mark_smob () const;

  Index_to_charcode_map const *
  get_index_to_charcode_map (const std::string &filename, int face_index,
                             FT_Face face);

  All_font_metrics (File_path search_path, All_font_metrics *previous);
  ~All_font_metrics ();

  Pango_font *find_pango_font (PangoFontDescription const *description,
                               bool is_emmentaler, Real scale);

  Open_type_font *find_otf_font (const std::string &name);
  SCM font_descriptions () const;

  void display_fonts ();
  std::string get_font_file (const std::string &name);
  void add_font_directory (const std::string &name);
  void add_font_file (const std::string &name);
};

extern All_font_metrics *all_fonts_global;

#endif /* ALL_FONTS_HH */
