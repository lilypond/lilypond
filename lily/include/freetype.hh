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

#ifndef FREETYPE_HH
#define FREETYPE_HH

// This include file loads the header file macros for FreeType.
#include <ft2build.h>
#include FT_FREETYPE_H

#include "lily-proto.hh"
#include "std-string.hh"
#include "box.hh"

void init_freetype ();
extern FT_Library freetype2_library;

std::string freetype_error_string (FT_Error code);

SCM box_to_scheme_lines (Box b);
Box ly_FT_get_unscaled_indexed_char_dimensions (FT_Face const &face,
                                                size_t signed_idx);
Box ly_FT_get_glyph_outline_bbox (FT_Face const &face, size_t signed_idx);
void ly_FT_add_outline_to_skyline (Lazy_skyline_pair *lazy,
                                   Transform const &transform,
                                   FT_Face const &face, size_t signed_idx);

#endif /* FREETYPE_HH */
