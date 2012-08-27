/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <freetype/ftoutln.h>
#include <freetype/ftbbox.h>

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
      return Box (Interval (infinity_f, -infinity_f), Interval (infinity_f, -infinity_f));
    }
  FT_Outline *outline;
  outline = &(face->glyph->outline);

  FT_BBox bbox;
  FT_Outline_Get_BBox (outline, &bbox);

  return Box (Interval (bbox.xMin, bbox.xMax), Interval (bbox.yMin, bbox.yMax));
}

SCM
ly_FT_get_glyph_outline (FT_Face const &face, size_t signed_idx)
{
  FT_UInt idx = FT_UInt (signed_idx);
  FT_Load_Glyph (face, idx, FT_LOAD_NO_SCALE);

  if (!(face->glyph->format == FT_GLYPH_FORMAT_OUTLINE))
    {
#if 0
      // will generate a lot of warnings
      warning ("Cannot make glyph outline");
#endif
      return box_to_scheme_lines (ly_FT_get_unscaled_indexed_char_dimensions (face, signed_idx));
    }

  FT_Outline *outline;
  outline = &(face->glyph->outline);
  SCM out = SCM_EOL;
  Offset lastpos;
  Offset firstpos;
  vsize j = 0;
  while (j < outline->n_points)
    {
      if (j == 0)
        {
          firstpos = Offset (outline->points[j].x, outline->points[j].y);
          lastpos = firstpos;
          j++;
        }
      else if (outline->tags[j] & 1)
        {
          // it is a line
          out = scm_cons (scm_list_4 (scm_from_double (lastpos[X_AXIS]),
                                      scm_from_double (lastpos[Y_AXIS]),
                                      scm_from_double (outline->points[j].x),
                                      scm_from_double (outline->points[j].y)),
                          out);
          lastpos = Offset (outline->points[j].x, outline->points[j].y);
          j++;
        }
      else if (outline->tags[j] & 2)
        {
          // it is a third order bezier
          out = scm_cons (scm_list_n (scm_from_double (lastpos[X_AXIS]),
                                      scm_from_double (lastpos[Y_AXIS]),
                                      scm_from_double (outline->points[j].x),
                                      scm_from_double (outline->points[j].y),
                                      scm_from_double (outline->points[j + 1].x),
                                      scm_from_double (outline->points[j + 1].y),
                                      scm_from_double (outline->points[j + 2].x),
                                      scm_from_double (outline->points[j + 2].y),
                                      SCM_UNDEFINED),
                          out);
          lastpos = Offset (outline->points[j + 2].x, outline->points[j + 2].y);
          j += 3;
        }
      else
        {
          // it is a second order bezier
          Real x0 = lastpos[X_AXIS];
          Real x1 = outline->points[j].x;
          Real x2 = outline->points[j + 1].x;

          Real y0 = lastpos[Y_AXIS];
          Real y1 = outline->points[j].y;
          Real y2 = outline->points[j + 1].y;

          Real qx2 = x0 + x2 - (2 * x1);
          Real qx1 = (2 * x1) - (2 * x0);
          Real qx0 = x0;

          Real qy2 = y0 + y2 - (2 * y1);
          Real qy1 = (2 * y1) - (2 * y0);
          Real qy0 = y0;

          Real cx0 = qx0;
          Real cx1 = qx0 + (qx1 / 3);
          Real cx2 = qx0 + (2 * qx1 / 3) + (qx2 / 3);
          Real cx3 = qx0 + qx1 + qx2;

          Real cy0 = qy0;
          Real cy1 = qy0 + (qy1 / 3);
          Real cy2 = qy0 + (2 * qy1 / 3) + (qy2 / 3);
          Real cy3 = qy0 + qy1 + qy2;

          out = scm_cons (scm_list_n (scm_from_double (cx0),
                                      scm_from_double (cy0),
                                      scm_from_double (cx1),
                                      scm_from_double (cy1),
                                      scm_from_double (cx2),
                                      scm_from_double (cy2),
                                      scm_from_double (cx3),
                                      scm_from_double (cy3),
                                      SCM_UNDEFINED),
                          out);
          lastpos = Offset (outline->points[j + 1].x, outline->points[j + 1].y);
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
