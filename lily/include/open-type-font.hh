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

#ifndef OPEN_TYPE_FONT_HH
#define OPEN_TYPE_FONT_HH

#include "font-metric.hh"

#include <unordered_map>
#include <utility>

Index_to_charcode_map make_index_to_charcode_map (FT_Face face);
void get_unicode_name (char *s, FT_ULong code);
void get_glyph_index_name (char *s, FT_ULong code);

struct Preinit_Open_type_font
{
  SCM lily_subfonts_;
  SCM lily_character_table_;
  SCM lily_global_table_;
  Preinit_Open_type_font ();
};

class Open_type_font : Preinit_Open_type_font, public Font_metric
{
  /* handle to face object */
  FT_Face face_;
  std::string postscript_name_;
  mutable std::unordered_map<std::string, size_t> name_to_index_map_;

  Index_to_charcode_map index_to_charcode_map_;
  mutable std::unordered_map<size_t, Box> lily_index_to_bbox_table_;
  std::string filename_;

  Open_type_font (FT_Face);

  OVERRIDE_CLASS_NAME (Open_type_font);

protected:
  void derived_mark () const override;

public:
  Real get_units_per_EM () const;
  SCM get_subfonts () const;
  SCM get_global_table () const;
  SCM get_char_table () const;
  SCM glyph_list () const;
  std::string const &filename () const { return filename_; }

  void add_outline_to_skyline (Lazy_skyline_pair *lazy, Transform const &tr,
                               size_t signed_idx) const;
  Box get_glyph_outline_bbox (size_t signed_idx) const;
  std::string get_otf_table (const std::string &tag) const;
  static SCM make_otf (const std::string &);
  std::string font_name () const override;
  ~Open_type_font ();
  std::pair<Offset, bool> attachment_point (const std::string &,
                                            Direction) const override;
  size_t count () const override;
  Box get_indexed_char_dimensions (size_t) const override;
  Box get_unscaled_indexed_char_dimensions (size_t) const;
  size_t name_to_index (std::string) const override;
  size_t index_to_charcode (size_t) const override;
  SCM sub_fonts () const override;
  Real design_size () const override;
};

std::string get_otf_table (FT_Face face, const std::string &tag);
FT_Face open_ft_face (const std::string &, FT_Long idx);
std::string get_postscript_name (FT_Face face);
std::string get_cff_name (FT_Face face);

#endif /* OPEN_TYPE_FONT_HH */
