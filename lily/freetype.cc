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

#include "freetype.hh"
#include "bezier.hh"
#include "lazy-skyline-pair.hh"
#include "international.hh"
#include "transform.hh"
#include "warn.hh"

#include FT_OUTLINE_H
#include FT_BBOX_H
#include FT_TRUETYPE_TAGS_H
#include FT_TRUETYPE_TABLES_H
#include FT_FONT_FORMATS_H

FT_Library freetype2_library;

void
init_freetype ()
{
  FT_Error errorcode = FT_Init_FreeType (&freetype2_library);
  if (errorcode)
    error (_ ("cannot initialize FreeType"));
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
  return Box (
    Interval (static_cast<Real> (hb), static_cast<Real> (hb + m.width)),
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

Offset
ftvector2offset (FT_Vector const &vec)
{
  return Offset (static_cast<Real> (vec.x), static_cast<Real> (vec.y));
}

struct Path_interpreter
{
  Lazy_skyline_pair *skyline_;
  Offset cur_;
  Transform transform_;
  Orientation orientation_;

  Path_interpreter (Lazy_skyline_pair *lazy, Transform t,
                    Orientation orientation)
  {
    skyline_ = lazy;
    transform_ = t;
    orientation_ = orientation;
  }
  int moveto (FT_Vector const &to)
  {
    cur_ = ftvector2offset (to);
    return 0;
  }
  int lineto (FT_Vector const &to)
  {
    Offset dest = ftvector2offset (to);
    skyline_->add_contour_segment (transform_, orientation_, cur_, dest);
    cur_ = dest;
    return 0;
  }
  int curve2to (FT_Vector const &control, FT_Vector const &to)
  {
    // It is a second order bezier.  We don't have code to
    // handle these. Substitute a line segment instead.
    (void) control;
    return lineto (to);
  }
  int curve3to (FT_Vector const &c1, FT_Vector const &c2, FT_Vector const &to)
  {
    Bezier curve;
    curve.control_[0] = cur_;
    curve.control_[1] = ftvector2offset (c1);
    curve.control_[2] = ftvector2offset (c2);
    curve.control_[3] = ftvector2offset (to);
    Offset start = transform_ (curve.control_[0]);
    Offset end = transform_ (curve.control_[3]);
    size_t quantization = std::max (2, int ((end - start).length () / 0.2));

    for (vsize i = 1; i < quantization; i++)
      {
        // This would be faster if we did the Bezier computation in integers.
        Offset pt = curve.curve_point (static_cast<Real> (i)
                                       / static_cast<Real> (quantization));
        skyline_->add_contour_segment (transform_, orientation_, cur_, pt);
        cur_ = pt;
      }
    skyline_->add_segment (transform_, cur_, curve.control_[3]);
    cur_ = curve.control_[3];
    return 0;
  }
  FT_Outline_Funcs funcs () const
  {
    FT_Outline_Funcs funcs {};
    funcs.move_to = [] (const FT_Vector *to, void *user) {
      return static_cast<Path_interpreter *> (user)->moveto (*to);
    };
    funcs.line_to = [] (const FT_Vector *to, void *user) {
      return static_cast<Path_interpreter *> (user)->lineto (*to);
    };
    funcs.conic_to = [] (const FT_Vector *c1, const FT_Vector *to, void *user) {
      return static_cast<Path_interpreter *> (user)->curve2to (*c1, *to);
    };
    funcs.cubic_to = [] (const FT_Vector *c1, const FT_Vector *c2,
                         const FT_Vector *to, void *user) {
      return static_cast<Path_interpreter *> (user)->curve3to (*c1, *c2, *to);
    };
    funcs.shift = 0;
    funcs.delta = 0;
    return funcs;
  };
};

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

  // TrueType and PS fonts have opposite ideas about contour
  // orientation.
  bool is_tt = std::string ("TrueType") == FT_Get_Font_Format (face);
  auto orientation = is_tt ? Orientation::CW : Orientation::CCW;

  Path_interpreter interpreter (lazy, transform, orientation);
  FT_Outline *outline = &(face->glyph->outline);
  FT_Outline_Funcs funcs = interpreter.funcs ();
  int err = FT_Outline_Decompose (outline, &funcs, &interpreter);
  assert (err == 0);
}
