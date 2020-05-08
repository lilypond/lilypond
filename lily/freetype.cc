/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "freetype.hh"
#include "lazy-skyline-pair.hh"
#include "transform.hh"
#include "warn.hh"

#include FT_OUTLINE_H
#include FT_BBOX_H

FT_Library freetype2_library;

void
init_freetype ()
{
  FT_Error errorcode = FT_Init_FreeType (&freetype2_library);
  if (errorcode)
    error ("cannot initialize FreeType");
}

Box
ly_FT_get_unscaled_indexed_char_dimensions (FT_Face const &face, size_t signed_idx)
{
  FT_UInt idx = FT_UInt (signed_idx);
  FT_Load_Glyph (face, idx, FT_LOAD_NO_SCALE);

  FT_Glyph_Metrics m = face->glyph->metrics;
  FT_Pos hb = m.horiBearingX;
  FT_Pos vb = m.horiBearingY;

  // is this viable for all grobs?
  return Box (Interval (static_cast<Real> (hb), static_cast<Real> (hb + m.width)),
              Interval (static_cast<Real> (vb - m.height), static_cast<Real> (vb)));
}

Box
ly_FT_get_glyph_outline_bbox (FT_Face const &face, size_t signed_idx)
{
  FT_UInt idx = FT_UInt (signed_idx);
  FT_Load_Glyph (face, idx, FT_LOAD_NO_SCALE);

  if (!(face->glyph->format == FT_GLYPH_FORMAT_OUTLINE))
    {
      // no warning; this happens a lot
      return Box (Interval (infinity_f, -infinity_f), Interval (infinity_f, -infinity_f));
    }
  FT_Outline *outline;
  outline = &(face->glyph->outline);

  FT_BBox bbox;
  FT_Outline_Get_BBox (outline, &bbox);

  return Box (Interval (static_cast<Real> (bbox.xMin),
                        static_cast<Real> (bbox.xMax)),
              Interval (static_cast<Real> (bbox.yMin),
                        static_cast<Real> (bbox.yMax)));
}

// (this is removed in a follow up change.)
void make_draw_bezier_boxes (Lazy_skyline_pair *skyline,
                             Transform const &transform, Real,
                             Offset control[4]);

void
ly_FT_add_outline_to_skyline (Lazy_skyline_pair *lazy,
                              Transform const &transform, FT_Face const &face,
                              size_t signed_idx)
{
  FT_UInt idx = FT_UInt (signed_idx);
  // We load the glyph unscaled; all returned outline coordinates are thus
  // not too large integers.
  FT_Load_Glyph (face, idx, FT_LOAD_NO_SCALE);

  if (!(face->glyph->format == FT_GLYPH_FORMAT_OUTLINE))
    {
      // no warnings; this happens a lot
      Box b = ly_FT_get_unscaled_indexed_char_dimensions (face, signed_idx);
      lazy->add_box (transform, b);
      return;
    }

  FT_Outline *outline = &(face->glyph->outline);
  Offset lastpos;
  Offset firstpos;
  int j = 0;
  while (j < outline->n_points)
    {
      if (j == 0)
        {
          firstpos = Offset (static_cast<Real> (outline->points[j].x),
                             static_cast<Real> (outline->points[j].y));
          lastpos = firstpos;
          j++;
        }
      else if (outline->tags[j] & 1)
        {
          // it is a line
          Offset p (static_cast<Real> (outline->points[j].x),
                    static_cast<Real> (outline->points[j].y));
          lazy->add_segment (transform, lastpos, p);
          lastpos = p;
          j++;
        }
      else if (outline->tags[j] & 2)
        {
          // it is a third order bezier
          Offset ps[4] = {lastpos};
          for (int i = 0; i < 3; i++)
            {
              ps[i + 1] = Offset (static_cast<Real> (outline->points[j + i].x),
                                  static_cast<Real> (outline->points[j + i].y));
            }
          lastpos = ps[3];
          make_draw_bezier_boxes (lazy, transform, 0.0, ps);
          j += 3;
        }
      else
        {
          // it is a second order bezier
          // we don't have code to handle these. Substitute a line segment instead.
          Offset p (static_cast<Real> (outline->points[j + 1].x),
                    static_cast<Real> (outline->points[j + 1].y));
          lazy->add_segment (transform, lastpos, p);
          lastpos = p;
          j += 2;
        }
    }

  lazy->add_segment (transform, lastpos, firstpos);
}
