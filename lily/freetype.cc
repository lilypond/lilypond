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
ly_FT_get_unscaled_indexed_char_dimensions (FT_Face const &face,
                                            size_t signed_idx)
{
  FT_UInt idx = FT_UInt (signed_idx);
  FT_Load_Glyph (face, idx, FT_LOAD_NO_SCALE);

  FT_Glyph_Metrics m = face->glyph->metrics;
  FT_Pos hb = m.horiBearingX;
  FT_Pos vb = m.horiBearingY;

  // is this viable for all grobs?
  return Box (Interval (Real (hb), Real (hb + m.width)),
              Interval (Real (vb - m.height), Real (vb)));
}

SCM
box_to_scheme_lines (Box b)
{
  return scm_list_4 (scm_list_4 (scm_from_double (b[X_AXIS][LEFT]),
                                 scm_from_double (b[Y_AXIS][DOWN]),
                                 scm_from_double (b[X_AXIS][RIGHT]),
                                 scm_from_double (b[Y_AXIS][DOWN])),
                     scm_list_4 (scm_from_double (b[X_AXIS][RIGHT]),
                                 scm_from_double (b[Y_AXIS][DOWN]),
                                 scm_from_double (b[X_AXIS][RIGHT]),
                                 scm_from_double (b[Y_AXIS][UP])),
                     scm_list_4 (scm_from_double (b[X_AXIS][RIGHT]),
                                 scm_from_double (b[Y_AXIS][UP]),
                                 scm_from_double (b[X_AXIS][LEFT]),
                                 scm_from_double (b[Y_AXIS][UP])),
                     scm_list_4 (scm_from_double (b[X_AXIS][LEFT]),
                                 scm_from_double (b[Y_AXIS][UP]),
                                 scm_from_double (b[X_AXIS][LEFT]),
                                 scm_from_double (b[Y_AXIS][DOWN])));
}

Box
ly_FT_get_glyph_outline_bbox (FT_Face const &face, size_t signed_idx)
{
  FT_UInt idx = FT_UInt (signed_idx);
  FT_Load_Glyph (face, idx, FT_LOAD_NO_SCALE);

  if (!(face->glyph->format == FT_GLYPH_FORMAT_OUTLINE))
    {
#if 0
      // will generate a lot of warnings
      warning ("Cannot make glyph outline");
#endif
      return Box (Interval (infinity_f, -infinity_f),
                  Interval (infinity_f, -infinity_f));
    }
  FT_Outline *outline;
  outline = &(face->glyph->outline);

  FT_BBox bbox;
  FT_Outline_Get_BBox (outline, &bbox);

  return Box (
      Interval (static_cast<Real> (bbox.xMin), static_cast<Real> (bbox.xMax)),
      Interval (static_cast<Real> (bbox.yMin), static_cast<Real> (bbox.yMax)));
}

SCM
ly_FT_get_glyph_outline (FT_Face const &face, size_t signed_idx)
{
  FT_UInt idx = FT_UInt (signed_idx);
  // We load the glyph unscaled; all returned outline coordinates are thus
  // not too large integers.
  FT_Load_Glyph (face, idx, FT_LOAD_NO_SCALE);

  if (!(face->glyph->format == FT_GLYPH_FORMAT_OUTLINE))
    {
#if 0
      // will generate a lot of warnings
      warning ("Cannot make glyph outline");
#endif
      return box_to_scheme_lines (
          ly_FT_get_unscaled_indexed_char_dimensions (face, signed_idx));
    }

  FT_Outline *outline;
  outline = &(face->glyph->outline);
  SCM out = SCM_EOL;
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
          out = scm_cons (scm_list_4 (scm_from_double (lastpos[X_AXIS]),
                                      scm_from_double (lastpos[Y_AXIS]),
                                      scm_from_long (outline->points[j].x),
                                      scm_from_long (outline->points[j].y)),
                          out);
          lastpos = Offset (static_cast<Real> (outline->points[j].x),
                            static_cast<Real> (outline->points[j].y));
          j++;
        }
      else if (outline->tags[j] & 2)
        {
          // it is a third order bezier
          out = scm_cons (scm_list_n (scm_from_double (lastpos[X_AXIS]),
                                      scm_from_double (lastpos[Y_AXIS]),
                                      scm_from_long (outline->points[j].x),
                                      scm_from_long (outline->points[j].y),
                                      scm_from_long (outline->points[j + 1].x),
                                      scm_from_long (outline->points[j + 1].y),
                                      scm_from_long (outline->points[j + 2].x),
                                      scm_from_long (outline->points[j + 2].y),
                                      SCM_UNDEFINED),
                          out);
          lastpos = Offset (static_cast<Real> (outline->points[j + 2].x),
                            static_cast<Real> (outline->points[j + 2].y));
          j += 3;
        }
      else
        {
          // it is a second order bezier
          Real x0 = lastpos[X_AXIS];
          Real x1 = static_cast<Real> (outline->points[j].x);
          Real x2 = static_cast<Real> (outline->points[j + 1].x);

          Real y0 = lastpos[Y_AXIS];
          Real y1 = static_cast<Real> (outline->points[j].y);
          Real y2 = static_cast<Real> (outline->points[j + 1].y);

          out = scm_cons (scm_list_n (scm_from_double (x0),
                                      scm_from_double (y0),
                                      scm_from_double ((x0 + 2 * x1) / 3),
                                      scm_from_double ((y0 + 2 * y1) / 3),
                                      scm_from_double ((2 * x1 + x2) / 3),
                                      scm_from_double ((2 * y1 + y2) / 3),
                                      scm_from_double (x2),
                                      scm_from_double (y2), SCM_UNDEFINED),
                          out);
          lastpos = Offset (static_cast<Real> (outline->points[j + 1].x),
                            static_cast<Real> (outline->points[j + 1].y));
          j += 2;
        }
    }

  // just in case, close the figure
  out = scm_cons (scm_list_4 (scm_from_double (lastpos[X_AXIS]),
                              scm_from_double (lastpos[Y_AXIS]),
                              scm_from_double (firstpos[X_AXIS]),
                              scm_from_double (firstpos[Y_AXIS])),
                  out);

  out = scm_reverse_x (out, SCM_EOL);
  return out;
}
