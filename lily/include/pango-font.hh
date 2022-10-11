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

#ifndef PANGO_FONT_HH
#define PANGO_FONT_HH

#include "config.hh"

#include <pango/pango.h>
#include <pango/pangoft2.h>

#include "font-metric.hh"

struct Preinit_Pango_font
{
  SCM physical_font_tab_;
  Preinit_Pango_font ();
};

class Pango_font : Preinit_Pango_font, public Font_metric
{
  PangoContext *context_;
  PangoFontDescription *pango_description_;
  Real scale_;

  SCM get_glyph_desc (PangoGlyphInfo const &pgi, Box const &scaled_extent,
                      std::string const &file_name, FT_Face ftface,
                      bool *cid_keyed) const;

public:
  SCM physical_font_tab () const;
  Pango_font (PangoFT2FontMap *, PangoFontDescription const *, Real);
  ~Pango_font ();
  OVERRIDE_CLASS_NAME (Pango_font);

  std::string description_string () const;
  SCM font_file_name () const override;
  void register_font_file (const std::string &filename,
                           const std::string &ps_name, int face_index);

  size_t name_to_index (std::string) const override;
  void add_outline_to_skyline (Lazy_skyline_pair *lazy, Transform const &tr,
                               size_t signed_idx) const;
  Box get_glyph_outline_bbox (size_t signed_idx) const;
  Box get_unscaled_indexed_char_dimensions (size_t) const;
  Box get_scaled_indexed_char_dimensions (size_t) const;

  Stencil pango_item_string_stencil (PangoGlyphItem const *,
                                     std::string const &text) const;

  Stencil text_stencil (Output_def *output_state, const std::string &text,
                        bool music,
                        const std::string &features_str) const override;
  void derived_mark () const override;
};

PangoFontDescription *symbols_to_pango_font_description (SCM family, SCM style,
                                                         SCM variant,
                                                         SCM weight,
                                                         SCM stretch);

Font_metric *select_pango_font (Output_def *layout, SCM chain);

const int PANGO_RESOLUTION = 1200;
PangoFontDescription *properties_to_pango_description (SCM chain,
                                                       Real text_size);

#endif /* PANGO_FONT_HH */
