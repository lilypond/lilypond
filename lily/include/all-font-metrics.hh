/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "font-metric.hh"
#include "config.hh"

#if HAVE_PANGO_FT2
#include <pango/pango.h>
#include <pango/pangoft2.h>
#endif


/*
   Interface to all fonts (both system fonts and fonts loaded
   via Pango).
*/
class All_font_metrics
{
  Scheme_hash_table *otf_dict_;
  File_path search_path_;

#if HAVE_PANGO_FT2
  PangoFT2FontMap *pango_ft2_fontmap_;
  Scheme_hash_table *pango_dict_;
  int pango_dpi_;
#endif

  map<string, Index_to_charcode_map > filename_charcode_maps_map_;
  
  All_font_metrics (All_font_metrics const &);
public:

  Index_to_charcode_map const *get_index_to_charcode_map (string filename,
							  int face_index,
							  FT_Face face);

  All_font_metrics (string search_path);
  ~All_font_metrics ();

  Pango_font *find_pango_font (PangoFontDescription const *description,
			       Real scale);

  Font_metric *find_font (string name);
  Open_type_font *find_otf (string name);
  SCM font_descriptions () const;
};

extern All_font_metrics *all_fonts_global;
SCM ly_reset_all_fonts ();

#endif /* ALL_FONTS_HH */
