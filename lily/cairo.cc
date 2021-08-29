/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021 Knut Petersen <knupero@gmail.com>

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

#include "config.hh"
#include "lily-guile.hh"
#include "warn.hh"

#if CAIRO_BACKEND

#include "cpu-timer.hh"
#include "dimensions.hh"
#include "file-name.hh"
#include "file-path.hh"
#include "freetype.hh"
#include "grob.hh"
#include "international.hh"
#include "lily-version.hh"
#include "ly-module.hh"
#include "main.hh"
#include "modified-font-metric.hh"
#include "open-type-font.hh"
#include "output-def.hh"
#include "pango-font.hh"
#include "paper-book.hh"
#include "prob.hh"
#include "program-option.hh"
#include "std-vector.hh"
#include "stencil-interpret.hh"
#include "stream-event.hh"
#include "string-convert.hh"

#include <cairo-ft.h>
#include <cairo-pdf.h>
#include <cairo-ps.h>
#include <cairo-svg.h>
#include <cairo.h>
#include <glib.h>

#include <png.h>

#include <algorithm>
#include <functional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

enum Cairo_output_format
{
  UNKNOWN = 0,
  PDF,
  SVG,
  PNG,
  EPS,
  PS,
};

static std::unordered_map<std::string, Cairo_output_format> output_formats = {
  {"svg", SVG}, {"pdf", PDF}, {"eps", EPS}, {"ps", PS}, {"png", PNG},
};

std::string
format_name (Cairo_output_format f)
{
  for (auto entry : output_formats)
    {
      if (entry.second == f)
        {
          return entry.first;
        }
    }

  abort ();
}

static bool
single_page_format (Cairo_output_format fmt)
{
  switch (fmt)
    {
    case EPS:
    case SVG:
    case PNG:
      return true;
    default:
      return false;
    }
}

Cairo_output_format
parse_format (std::string const &f)
{
  return output_formats[f];
}

struct Pair_hash
{
  template <class T1, class T2>
  std::size_t operator() (const std::pair<T1, T2> &pair) const
  {
    return std::hash<T1> () (pair.first) ^ std::hash<T2> () (pair.second);
  }
};

class Cairo_outputter : public Stencil_sink
{
  // (filename, index) => FT_Face (owned)
  std::unordered_map<std::pair<std::string, int>, FT_Face, Pair_hash> ft_faces_;
  FT_Face ft_font (std::string const &file, int index);

  // Keys unowned, values owned.
  std::unordered_map<FT_Face, cairo_font_face_t *> cairo_fonts_;
  cairo_font_face_t *cairo_font_for_ft_font (FT_Face face);

  Cairo_output_format format_;

  // Transform staff-space units to Cairo bigpoints
  Real scale_factor_;

  cairo_surface_t *surface_ = nullptr;
  cairo_t *context_ = nullptr;

  std::string outfile_basename_;
  std::string filename_;

  SCM point_and_click_;

  // png specific data
  unsigned int png_height_;
  unsigned int png_width_;

  SCM output (SCM scm) override;
  void check_errors ();
  void png_write ();

  // drawing routines:
  void show_named_glyph (SCM scaledname, SCM glyphname);
  void print_glyphs (SCM size, SCM glyphs, SCM filename, SCM index);
  void path (SCM thickness, SCM exps, SCM cap, SCM join, SCM filled);
  void moveto (SCM varx, SCM vary);
  void setrgbacolor (SCM varr, SCM varg, SCM varb, SCM vara);

  void resetrgbacolor ();
  void draw_maybe_filled_path (bool filled, Real blot);
  void draw_line (SCM blotdiam, SCM xa, SCM ya, SCM xb, SCM yb);
  void draw_dashed_line (SCM blotdiam, SCM paton, SCM patoff, SCM vardx,
                         SCM vardy, SCM phase);

  void draw_round_box (SCM left, SCM right, SCM bottom, SCM top, SCM blotdiam);

  void draw_polygon (SCM points, SCM linewidth, SCM filled);

  void draw_circle (SCM radius, SCM thickness, SCM filled);

  void draw_ellipse (SCM xradius, SCM yradius, SCM thickness, SCM filled);

  void draw_partial_ellipse (SCM xradius, SCM yradius, SCM startangle,
                             SCM endangle, SCM thickness, SCM connected,
                             SCM filled);

  void set_rotation (SCM angle, SCM varx, SCM vary);
  void reset_rotation ();
  std::string pdf_rect (Real llx, Real lly, Real w, Real h,
                        bool relative_to_current) const;
  void cairo_link (std::string const &attr);
  void url_link (SCM target, SCM varx, SCM vary);
  void url_link (std::string const &target, Real llx, Real lly, Real w, Real h,
                 bool relative);
  void textedit_link (Real llx, Real lly, Real w, Real h,
                      std::string const &file, int line, int scol, int ecol);
  void grob_cause (SCM, SCM);
  void page_link (SCM target, SCM varx, SCM vary);
  void set_scale (SCM varx, SCM vary);
  void reset_scale ();
  void metadata (std::string const &key, std::string const &val,
                 bool user_provided);

public:
  Cairo_outputter (Cairo_output_format format, std::string const &basename,
                   Output_def *paper);
  ~Cairo_outputter ();
  void create_surface (Stencil const *, int);
  void finish_page ();
  void handle_metadata (SCM header);
  void close ();
};

void
Cairo_outputter::check_errors ()
{
  // Cairo can still gobble I/O errors for PDF (prior to commmit
  // 2fbd53a6b3, 2021/07/23) and SVG (prior to commmit 4c6b604bd5, 2021/07/24).
  if (context_)
    {
      auto status = cairo_status (context_);
      if (status != CAIRO_STATUS_SUCCESS)
        warning (
          _f ("Cairo context status '%s'", cairo_status_to_string (status)));
    }

  if (surface_)
    {
      auto status = cairo_surface_status (surface_);
      if (status != CAIRO_STATUS_SUCCESS)
        warning (
          _f ("Cairo surface status '%s'", cairo_status_to_string (status)));
    }
}

void
png_error (png_structp, png_const_charp err_msg)
{
  if (err_msg)
    error (_f ("libpng error: '%s'", err_msg));
  else
    error (_f ("libpng error, no details given."));
}

void
Cairo_outputter::png_write ()
{
  unsigned char *data = cairo_image_surface_get_data (surface_);
  png_structp png = png_create_write_struct (PNG_LIBPNG_VER_STRING, nullptr,
                                             png_error, png_error);
  if (!png)
    error ("png_create_write_struct() failed");

  png_image image = {};
  image.version = PNG_IMAGE_VERSION;
  image.width = png_width_;
  image.height = png_height_;
  image.format = PNG_FORMAT_RGBA;

  if (!png_image_write_to_file (&image, filename_.c_str (), 0, data, 0, NULL))
    {
      error (_f ("error writing %s", filename_.c_str ()));
    }
  png_destroy_write_struct (&png, NULL);
}

cairo_font_face_t *
Cairo_outputter::cairo_font_for_ft_font (FT_Face face)
{
  cairo_font_face_t *cairo_font_face = cairo_fonts_[face];
  if (!cairo_font_face)
    {
      static const cairo_user_data_key_t ukey = {};
      cairo_font_face = cairo_ft_font_face_create_for_ft_face (face, 0);
      cairo_fonts_[face] = cairo_font_face;

      // We get a mixture of fonts owned by us (text fonts) and owned
      // externally (the music font). At the same time, Cairo holds on
      // to an internal cache of cairo_font_face_ts and their
      // associated FT_Face, which may live past the Cairo context and
      // surface.  This means we can't simply discard all the owned
      // fonts in the destructor, because Cairo may reuse them for the
      // next document or next page, leading to odd artifacts.  We
      // solve this by handling FT_Face lifetime with a finalizer
      // (cairo_destroy_func_t), that is balanced with a refcount
      // increase.  The call to open_ft_face for owned text fonts is
      // balanced with FT_Done_Face calls in ~Cairo_outputter.
      FT_Reference_Face (face);

      if (cairo_font_face_set_user_data (cairo_font_face, &ukey, face,
                                         (cairo_destroy_func_t) FT_Done_Face))
        {
          programming_error ("cairo_font_face_set_user_data failed");
        }
    }

  return cairo_font_face;
}

FT_Face
Cairo_outputter::ft_font (std::string const &file, int index)
{
  std::pair<std::string, int> key = {file, index};
  auto it = ft_faces_.find (key);
  if (it != ft_faces_.end ())
    return it->second;

  FT_Face f = open_ft_face (file, index);
  ft_faces_[key] = f;
  return f;
}

void
Cairo_outputter::show_named_glyph (SCM scaled_font, SCM glyphname)
{
  auto *const mfm = LY_ASSERT_SMOB (Modified_font_metric, scaled_font, 1);
  std::string g = ly_scm2string (glyphname);

  Font_metric *orig = mfm->original_font ();
  auto otf = dynamic_cast<Open_type_font *> (orig);
  assert (otf);

  Real font_scale_factor_ = mfm->magnification () * otf->design_size ();

  cairo_set_font_face (context_,
                       cairo_font_for_ft_font (otf->freetype_handle ()));
  cairo_matrix_t m = {
    .xx = font_scale_factor_,
    .yx = 0,
    .xy = 0,
    .yy = -font_scale_factor_,
    .x0 = 0,
    .y0 = 0,
  };
  cairo_set_font_matrix (context_, &m);

  Real cx, cy;
  cairo_get_current_point (context_, &cx, &cy);
  cairo_glyph_t oneglyph
    = {FT_Get_Name_Index (otf->freetype_handle (), (FT_String *) g.c_str ()),
       cx, cy};

  cairo_show_glyphs (context_, &oneglyph, 1);
}

void
Cairo_outputter::print_glyphs (SCM size, SCM glyphs, SCM filename,
                               SCM face_index)
{
  Real sumw = 0.0;

  Real startx, starty;
  cairo_get_current_point (context_, &startx, &starty);

  // TODO - it would be nice to reuse the FT_Face instance that Pango
  // used to create the glyph string.
  FT_Face ft_face
    = ft_font (ly_scm2string (filename), from_scm<int> (face_index));
  cairo_set_font_face (context_, cairo_font_for_ft_font (ft_face));

  // TODO - why do we need to scale with scale_factor here?
  Real scale = from_scm<Real> (size) / (bigpoint_constant * scale_factor_);
  cairo_matrix_t m = {
    .xx = scale,
    .yx = 0,
    .xy = 0,
    .yy = -scale,
    .x0 = 0,
    .y0 = 0,
  };
  cairo_set_font_matrix (context_, &m);

  std::vector<cairo_glyph_t> cairo_glyphs;
  std::string utf8;
  for (SCM g = glyphs; scm_is_pair (g); g = scm_cdr (g))
    {
      SCM whxyg = scm_car (g);
      Real w = from_scm<Real> (scm_car (whxyg));
      Real x = from_scm<Real> (scm_caddr (whxyg));
      Real y = from_scm<Real> (scm_cadddr (whxyg));
      SCM glyph_scm = scm_cadddr (scm_cdr (whxyg));
      if (!FT_HAS_GLYPH_NAMES (ft_face))
        {
          if (scm_is_string (glyph_scm))
            {
              // We have a font without glyph names. LilyPond provides the
              // glyph either as a string "uniXXXX" where XXXX is a four-digit
              // hex number or as a string "uXXXXX..." for hex numbers greater
              // than 0xffff (See get_glyph_name() in pango-font.cc).
              // This code path is currently not covered by the regression test.
              std::string hex;
              std::string glyph_ustr = ly_scm2string (glyph_scm);
              if (!glyph_ustr.rfind ("uni", 0))
                hex = glyph_ustr.substr (3);
              else
                hex = glyph_ustr.substr (1);
              unsigned long gb = std::stoul (hex, nullptr, 16);
              char gc[] = {0, 0, 0, 0, 0, 0};
              int n = g_unichar_to_utf8 ((gunichar) gb, gc);
              utf8 += std::string (gc, n);
            }
          else
            {
              // We have a font without glyph names.
              // The glyph is not a string, so it definitely must be an
              // unsigned int. We feed this number to cairo_show_glyphs, people
              // will complain if this is wrong. Tested to work correctly with
              // Google Noto CJK OpenType/CFF Collection (OTC) font. Our test
              // case collection does not exercise this code.
              int glyph_code = from_scm<int> (glyph_scm);
              cairo_glyphs.push_back (cairo_glyph_t ({
                .index = static_cast<long unsigned int> (glyph_code),
                .x = startx + (x + sumw),
                .y = starty - y,
              }));
            }
        }
      else // we have a font with glyph names
        {
          std::string g = ly_scm2string (glyph_scm);
          cairo_glyphs.push_back (cairo_glyph_t ({
            .index = FT_Get_Name_Index (ft_face, (FT_String *) g.c_str ()),
            .x = startx + (x + sumw),
            .y = starty - y,
          }));
        }
      sumw = sumw + w;
    }

  assert (cairo_glyphs.empty () || utf8.empty ());
  if (!cairo_glyphs.empty ())
    {
      cairo_show_glyphs (context_, cairo_glyphs.data (),
                         int (cairo_glyphs.size ()));
    }
  else if (!utf8.empty ())
    {
      cairo_show_text (context_, utf8.c_str ());
    }
}

void
Cairo_outputter::path (SCM thickness, SCM exps, SCM cap, SCM join, SCM filled)
{
  // Set linewidth
  Real blot = from_scm<Real> (thickness);

  cairo_set_line_width (context_, blot);

  if (scm_is_eq (cap, ly_symbol2scm ("butt")))
    cairo_set_line_cap (context_, CAIRO_LINE_CAP_BUTT);
  else if (scm_is_eq (cap, ly_symbol2scm ("square")))
    cairo_set_line_cap (context_, CAIRO_LINE_CAP_SQUARE);
  else
    {
      if (!SCM_UNBNDP (cap) && !scm_is_eq (cap, ly_symbol2scm ("round")))
        warning (_f ("unknown line-cap-style: %s",
                     ly_scm_write_string (cap).c_str ()));

      cairo_set_line_cap (context_, CAIRO_LINE_CAP_ROUND);
    }

  if (scm_is_eq (join, ly_symbol2scm ("miter")))
    cairo_set_line_join (context_, CAIRO_LINE_JOIN_MITER);
  else if (scm_is_eq (join, ly_symbol2scm ("bevel")))
    cairo_set_line_join (context_, CAIRO_LINE_JOIN_BEVEL);
  else
    {
      if (!SCM_UNBNDP (join) && !scm_is_eq (join, ly_symbol2scm ("round")))
        warning (_f ("unknown line-cap-style: %s",
                     ly_scm_write_string (cap).c_str ()));
      cairo_set_line_join (context_, CAIRO_LINE_JOIN_ROUND);
    }

  // save to be able to undo cairo_translate
  cairo_save (context_);

  // translate: current point is new cairo origin
  Real cpx, cpy;
  cairo_get_current_point (context_, &cpx, &cpy);

  cairo_translate (context_, cpx, cpy);
  // evaluate drawing primitives given in exps
  while (scm_is_pair (exps))
    {
      SCM head = scm_car (exps);
      if (scm_is_eq (head, ly_symbol2scm ("moveto")))
        {
          cairo_move_to (context_, from_scm<Real> (scm_cadr (exps)),
                         from_scm<Real> (scm_caddr (exps)));
          exps = scm_cdddr (exps);
        }
      else if (scm_is_eq (head, ly_symbol2scm ("rmoveto")))
        {
          cairo_rel_move_to (context_, from_scm<Real> (scm_cadr (exps)),
                             from_scm<Real> (scm_caddr (exps)));
          exps = scm_cdddr (exps);
        }
      else if (scm_is_eq (head, ly_symbol2scm ("lineto")))
        {
          cairo_line_to (context_, from_scm<Real> (scm_cadr (exps)),
                         from_scm<Real> (scm_caddr (exps)));
          exps = scm_cdddr (exps);
        }
      else if (scm_is_eq (head, ly_symbol2scm ("rlineto")))
        {
          cairo_rel_line_to (context_, from_scm<Real> (scm_cadr (exps)),
                             from_scm<Real> (scm_caddr (exps)));
          exps = scm_cdddr (exps);
        }
      else if (scm_is_eq (head, ly_symbol2scm ("curveto")))
        {
          cairo_curve_to (context_, from_scm<Real> (scm_cadr (exps)),
                          from_scm<Real> (scm_caddr (exps)),
                          from_scm<Real> (scm_cadddr (exps)),
                          from_scm<Real> (scm_cadddr (scm_cdr (exps))),
                          from_scm<Real> (scm_cadddr (scm_cddr (exps))),
                          from_scm<Real> (scm_cadddr (scm_cdddr (exps))));
          exps = scm_cddddr (scm_cdddr (exps));
        }
      else if (scm_is_eq (head, ly_symbol2scm ("rcurveto")))
        {
          cairo_rel_curve_to (context_, from_scm<Real> (scm_cadr (exps)),
                              from_scm<Real> (scm_caddr (exps)),
                              from_scm<Real> (scm_cadddr (exps)),
                              from_scm<Real> (scm_cadddr (scm_cdr (exps))),
                              from_scm<Real> (scm_cadddr (scm_cddr (exps))),
                              from_scm<Real> (scm_cadddr (scm_cdddr (exps))));
          exps = scm_cddddr (scm_cdddr (exps));
        }
      else if (scm_is_eq (head, ly_symbol2scm ("closepath")))
        {
          cairo_close_path (context_);
          exps = scm_cdr (exps);
        }
      else
        {
          programming_error ("unexpected path operator: "
                             + ly_scm_write_string (head));
        }
    }

  // stroke / fill according to user wishes
  bool fill = false;
  if (!SCM_UNBNDP (filled))
    {
      LY_ASSERT_TYPE (scm_is_bool, filled, 5);
      fill = from_scm<bool> (filled);
    }

  draw_maybe_filled_path (fill, blot);

  // undo context_->translate
  cairo_restore (context_);
}

void
Cairo_outputter::finish_page ()
{
  cairo_show_page (context_);
  check_errors ();
  if (single_page_format (format_))
    {
      cairo_surface_flush (surface_);
      if (format_ == PNG)
        png_write ();
      cairo_surface_finish (surface_);
      check_errors ();
      cairo_surface_destroy (surface_);
      cairo_destroy (context_);
      surface_ = nullptr;
      context_ = nullptr;
    }
}

void
Cairo_outputter::create_surface (Stencil const *stencil, int page_number)
{
  filename_ = outfile_basename_;
  if (single_page_format (format_))
    filename_ += "-" + std::to_string (page_number);
  filename_ += "." + format_name (format_);

  message (_f ("Output to `%s'...\n", filename_.c_str ()));

  Real paper_width = stencil->extent (X_AXIS).length ();
  Real paper_height = stencil->extent (Y_AXIS).length ();

  paper_width *= scale_factor_;
  paper_height *= scale_factor_;

  switch (format_)
    {
    case SVG:
      surface_ = cairo_svg_surface_create (filename_.c_str (), paper_width,
                                           paper_height);
      context_ = cairo_create (surface_);
      break;
    case PDF:
      surface_ = cairo_pdf_surface_create (filename_.c_str (), paper_width,
                                           paper_height);
      context_ = cairo_create (surface_);
      break;
    case PNG:
      {
        int png_dpi
          = from_scm<int> (ly_get_option (ly_symbol2scm ("resolution")));
        png_height_ = std::max ((int) round (paper_height / 72.0 * png_dpi), 1);
        png_width_ = std::max ((int) round (paper_width / 72.0 * png_dpi), 1);
        surface_ = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, png_width_,
                                               png_height_);
        context_ = cairo_create (surface_);
        cairo_scale (context_, png_dpi / 72.0, png_dpi / 72.0);

        cairo_save (context_);
        // TODO - make transparency/background tunable. White
        // background is easier for visual inspection
        cairo_set_source_rgba (context_, 1, 1, 1, 1);
        cairo_paint (context_);
        cairo_restore (context_);
        break;
      }
    case PS:
    case EPS:
      surface_ = cairo_ps_surface_create (filename_.c_str (), paper_width,
                                          paper_height);
      cairo_ps_surface_set_eps (surface_, format_ == EPS);
      context_ = cairo_create (surface_);
      break;
    default:
      abort ();
    }

  cairo_scale (context_, scale_factor_, -scale_factor_);
}

void
Cairo_outputter::moveto (SCM varx, SCM vary)
{
  Real x = from_scm<Real> (varx);
  Real y = from_scm<Real> (vary);
  cairo_move_to (context_, x, y);
}

void
Cairo_outputter::setrgbacolor (SCM varr, SCM varg, SCM varb, SCM vara)
{
  Real r = from_scm<Real> (varr);
  Real g = from_scm<Real> (varg);
  Real b = from_scm<Real> (varb);
  Real a = SCM_UNBNDP (vara) ? 1.0 : from_scm<Real> (vara);

  cairo_save (context_);
  cairo_set_source_rgba (context_, r, g, b, a);
}

void
Cairo_outputter::resetrgbacolor ()
{
  cairo_restore (context_);
}

void
Cairo_outputter::draw_line (SCM blotdiam, SCM xa, SCM ya, SCM xb, SCM yb)
{
  Real d = from_scm<Real> (blotdiam);
  Real x = from_scm<Real> (xa);
  Real y = from_scm<Real> (ya);
  Real dx = from_scm<Real> (xb) - x;
  Real dy = from_scm<Real> (yb) - y;

  cairo_set_line_width (context_, d);
  cairo_set_line_cap (context_, CAIRO_LINE_CAP_ROUND);
  cairo_rel_move_to (context_, x, y);
  cairo_rel_line_to (context_, dx, dy);
  cairo_stroke (context_);
}

void
Cairo_outputter::draw_dashed_line (SCM blotdiam, SCM paton, SCM patoff,
                                   SCM vardx, SCM vardy, SCM phase)
{
  Real dx = from_scm<Real> (vardx);
  Real dy = from_scm<Real> (vardy);
  Real on = from_scm<Real> (paton);
  Real off = from_scm<Real> (patoff);
  Real pat[] = {on, off};

  cairo_save (context_);
  cairo_set_dash (context_, pat, 2, from_scm<Real> (phase));
  cairo_set_line_width (context_, from_scm<Real> (blotdiam));
  cairo_set_line_cap (context_, CAIRO_LINE_CAP_ROUND);
  cairo_rel_line_to (context_, dx, dy);
  cairo_stroke (context_);
  cairo_restore (context_);
}

static Real
deg_to_rad (Real a)
{
  return a * M_PI / 180.0;
}

void
Cairo_outputter::draw_round_box (SCM left, SCM right, SCM bottom, SCM top,
                                 SCM blotdiam)
{
  Real r = (from_scm<Real> (blotdiam)) / 2;
  Real x = r - (from_scm<Real> (left));
  Real y = r - (from_scm<Real> (bottom));
  Real w = (from_scm<Real> (right)) - r - x;
  Real h = (from_scm<Real> (top)) - r - y;
  // FIXME correct but inefficient code (pdfs are bigger than necessary)
  //       possible optimizations: see ps code in music-drawing-routines.ps
  if (r == 0)
    {
      cairo_rel_move_to (context_, x, y);
      cairo_rel_line_to (context_, 0, h);
      cairo_rel_line_to (context_, w, 0);
      cairo_rel_line_to (context_, 0, -h);
      cairo_rel_line_to (context_, -w, 0);
      cairo_close_path (context_);
      cairo_fill (context_);
    }
  else
    {
      cairo_rel_move_to (context_, x, y);

      Real cx, cy;
      cairo_get_current_point (context_, &cx, &cy);

      cairo_new_sub_path (context_);
      cairo_arc (context_, cx + w, cy, r, -M_PI / 2, 0);
      cairo_arc (context_, cx + w, cy + h, r, 0.0, M_PI / 2);
      cairo_arc (context_, cx, cy + h, r, M_PI / 2, M_PI);
      cairo_arc (context_, cx, cy, r, M_PI, M_PI * 1.5);
      cairo_close_path (context_);
      cairo_fill (context_);
    }
}

void
Cairo_outputter::draw_polygon (SCM points, SCM linewidth, SCM filled)
{
  Real cx, cy;
  cairo_get_current_point (context_, &cx, &cy);

  cairo_set_line_cap (context_, CAIRO_LINE_CAP_BUTT);
  cairo_set_line_join (context_, CAIRO_LINE_JOIN_ROUND);
  bool first = true;
  for (; scm_is_pair (points); points = scm_cddr (points))
    {
      Real x = from_scm<Real> (scm_car (points));
      Real y = from_scm<Real> (scm_cadr (points));
      if (first)
        cairo_move_to (context_, x + cx, y + cy);
      else
        cairo_line_to (context_, x + cx, y + cy);
      first = false;
    }

  cairo_close_path (context_);
  draw_maybe_filled_path (from_scm<bool> (filled), from_scm<Real> (linewidth));
}

void
Cairo_outputter::draw_maybe_filled_path (bool filled, Real blot)
{
  if (blot)
    cairo_set_line_width (context_, blot);
  if (filled)
    {
      if (blot)
        cairo_stroke_preserve (context_);
      cairo_fill (context_);
    }
  else
    cairo_stroke (context_);
}

void
Cairo_outputter::draw_circle (SCM radius, SCM thickness, SCM filled)
{
  Real rad = from_scm<Real> (radius);
  Real cx, cy;
  cairo_get_current_point (context_, &cx, &cy);

  cairo_new_sub_path (context_);
  cairo_arc (context_, cx, cy, rad, 0.0, 2 * M_PI);
  draw_maybe_filled_path (from_scm<bool> (filled), from_scm<Real> (thickness));
}

void
Cairo_outputter::draw_ellipse (SCM xradius, SCM yradius, SCM thickness,
                               SCM filled)
{
  Real xrad = from_scm<Real> (xradius);
  Real yrad = from_scm<Real> (yradius);
  Real cx, cy;
  cairo_get_current_point (context_, &cx, &cy);

  cairo_save (context_);
  cairo_translate (context_, cx, cy);
  cairo_scale (context_, 1, yrad / xrad);
  cairo_new_path (context_);
  cairo_arc (context_, 0, 0, xrad, 0, 2 * M_PI);
  cairo_restore (context_);
  draw_maybe_filled_path (from_scm<bool> (filled), from_scm<Real> (thickness));
}

void
Cairo_outputter::draw_partial_ellipse (SCM xradius, SCM yradius, SCM startangle,
                                       SCM endangle, SCM thickness,
                                       SCM connected, SCM filled)
{
  Real xrad = from_scm<Real> (xradius);
  Real yrad = from_scm<Real> (yradius);
  Real cx, cy;
  cairo_get_current_point (context_, &cx, &cy);

  cairo_save (context_);
  cairo_translate (context_, cx, cy);
  cairo_scale (context_, 1, yrad / xrad);
  cairo_new_path (context_);
  cairo_arc (context_, 0, 0, xrad, deg_to_rad (-from_scm<Real> (endangle)),
             deg_to_rad (-from_scm<Real> (startangle)));
  if (from_scm<bool> (connected))
    cairo_close_path (context_);

  cairo_restore (context_);
  draw_maybe_filled_path (from_scm<bool> (filled), from_scm<Real> (thickness));
}

void
Cairo_outputter::set_rotation (SCM angle, SCM varx, SCM vary)
{
  Real ang = from_scm<Real> (angle);
  Real x = from_scm<Real> (varx);
  Real y = from_scm<Real> (vary);

  cairo_save (context_);
  cairo_translate (context_, x, y);
  cairo_rotate (context_, deg_to_rad (ang));
  cairo_translate (context_, -x, -y);
}

void
Cairo_outputter::reset_rotation ()
{
  cairo_restore (context_);
}

void
Cairo_outputter::textedit_link (Real llx, Real lly, Real w, Real h,
                                std::string const &file, int line,
                                int byte_count, int col)
{
  /* stencil-interpret.cc passes the current offset as 1st grob-cause,
     so no need to get current offset. */
  url_link (String_convert::form_string ("textedit://%s:%d:%d:%d",
                                         file.c_str (), line, byte_count, col),
            llx, lly, w, h, false);
}

void
Cairo_outputter::url_link (SCM target, SCM x_interval, SCM y_interval)
{
  std::string url = ly_scm2string (target);
  auto x = from_scm<Interval> (x_interval);
  auto y = from_scm<Interval> (y_interval);

  url_link (ly_scm2string (target), x[LEFT], y[LEFT], x.length (), y.length (),
            true);
}

std::string
Cairo_outputter::pdf_rect (Real llx, Real lly, Real w, Real h,
                           bool relative_to_current) const
{
  Real cx = 0.0, cy = 0.0;
  if (relative_to_current)
    cairo_get_current_point (context_, &cx, &cy);
  return String_convert::form_string (
    "rect=[ %f %f %f %f ] ", (cx + llx) * scale_factor_,
    -(cy + lly + h) * scale_factor_, w * scale_factor_, h * scale_factor_);
}

void
Cairo_outputter::url_link (std::string const &target, Real llx, Real lly,
                           Real w, Real h, bool relative_to_current)
{
  if (std::isinf (llx) || std::isinf (lly) || std::isinf (w) || std::isinf (h))
    return;

  std::string attr = String_convert::form_string (
    "%s uri='%s'", pdf_rect (llx, lly, w, h, relative_to_current).c_str (),
    target.c_str ());

  cairo_link (attr);
}

void
Cairo_outputter::grob_cause (SCM offset, SCM grob_scm)
{
  if (!scm_is_true (point_and_click_))
    return;

  Grob *grob = unsmob<Grob> (grob_scm);
  if (!grob)
    return;

  SCM cause = get_property (grob, "cause");
  Stream_event *ev = unsmob<Stream_event> (cause);
  if (!ev)
    return;

  if (scm_is_symbol (point_and_click_))
    {
      if (!ev->internal_in_event_class (point_and_click_))
        return;
    }

  bool is_list = false, found_sym = false;
  for (SCM p = point_and_click_; !found_sym && scm_is_pair (p); p = scm_cdr (p))
    {
      is_list = true;
      if (ev->internal_in_event_class (scm_car (p)))
        found_sym = true;
    }

  if (is_list && !found_sym)
    return;
  SCM location = get_property (ev, "origin");
  Input *origin = unsmob<Input> (location);
  if (!origin)
    return;

  ssize_t line, chr, col, unused;
  origin->get_counts (&line, &chr, &col, &unused);

  File_name name (origin->file_string ());

  Offset off (from_scm<Offset> (offset));
  Interval x (grob->extent (grob, X_AXIS));
  Interval y (grob->extent (grob, Y_AXIS));
  if (x.is_empty () || y.is_empty ())
    return;

  textedit_link (off[X_AXIS] + x[LEFT], off[Y_AXIS] + y[DOWN], x.length (),
                 y.length (),
                 String_convert::percent_encode (
                   name.absolute (get_working_directory ()).to_string ()),
                 int (line), int (chr), int (col));
}

//
// PDF page links were introduced with cairo stable 1.16.0.
// Although not documented, it was known that page links with 'page' being
// a forward reference to a not already processed page were broken from
// introduction of the code up to cairo commit 099d71fb9f2 (2021/07/24).
// See: https://gitlab.freedesktop.org/cairo/cairo/-/issues/336 and the
// thread https://lists.cairographics.org/archives/cairo/2021-July/029282.html
//
// FIXME Should print a warning message?
//
void
Cairo_outputter::page_link (SCM target, SCM varx, SCM vary)
{
  if (!is_scm<int> (target))
    return;

  int page = from_scm<int> (target);
  Real x = from_scm<Real> (scm_car (varx));
  Real y = from_scm<Real> (scm_car (vary));
  Real w = from_scm<Real> (scm_cdr (varx)) - x;
  Real h = from_scm<Real> (scm_cdr (vary)) - y;
  std::string attr = String_convert::form_string (
    "%s page=%d pos=[0.0 0.0]", pdf_rect (x, y, w, h, true).c_str (), page);

  cairo_link (attr);
}

void
Cairo_outputter::cairo_link (std::string const &attr)
{
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 16, 0)
  cairo_tag_begin (context_, CAIRO_TAG_LINK, attr.c_str ());
  cairo_tag_end (context_, CAIRO_TAG_LINK);
#else
  (void) attr;

  static bool warned;
  if (!warned)
    {
      warning ("cairo library too old to support links");
      warned = true;
    }
#endif
}

void
Cairo_outputter::set_scale (SCM varx, SCM vary)
{
  Real x = from_scm<Real> (varx);
  Real y = from_scm<Real> (vary);

  cairo_save (context_);
  cairo_scale (context_, x, y);
}

void
Cairo_outputter::reset_scale ()
{
  cairo_restore (context_);
}

static std::unordered_map<std::string, cairo_pdf_metadata_t> metadata_keys = {
  {"author", CAIRO_PDF_METADATA_AUTHOR},
  {"creator", CAIRO_PDF_METADATA_CREATOR},
  {"keywords", CAIRO_PDF_METADATA_KEYWORDS},
  {"subject", CAIRO_PDF_METADATA_SUBJECT},
  {"title", CAIRO_PDF_METADATA_TITLE},
  {"modDate", CAIRO_PDF_METADATA_MOD_DATE},
  {"creationDate", CAIRO_PDF_METADATA_CREATE_DATE},
};

void
Cairo_outputter::metadata (std::string const &key, std::string const &val,
                           bool user_provided)
{
  if (cairo_surface_get_type (surface_) == CAIRO_SURFACE_TYPE_PDF)
    {
      auto it = metadata_keys.find (key);
      assert (it != metadata_keys.end ());
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 16, 0)
      cairo_pdf_surface_set_metadata (surface_, it->second, val.c_str ());
      (void) user_provided;
#else
      static bool warned;
      if (!warned && user_provided)
        {
          warning (_f ("Cairo too old for PDF metadata"));
          warned = true;
        }
      (void) val;
#endif
    }
}

void
Cairo_outputter::close ()
{
  if (surface_)
    {
      cairo_surface_finish (surface_);
      check_errors ();
    }
}

Cairo_outputter::~Cairo_outputter ()
{
  if (surface_)
    {
      cairo_surface_destroy (surface_);
    }
  if (context_)
    {
      cairo_destroy (context_);
    }

  for (auto f : cairo_fonts_)
    {
      cairo_font_face_destroy (f.second);
    }
  for (auto f : ft_faces_)
    {
      FT_Done_Face (f.second);
    }
}

Cairo_outputter::Cairo_outputter (Cairo_output_format format,
                                  std::string const &basename,
                                  Output_def *paper)
{
  point_and_click_ = ly_get_option (ly_symbol2scm ("point-and-click"));
  format_ = format;
  outfile_basename_ = basename;

  scale_factor_
    = paper->get_dimension (ly_symbol2scm ("output-scale")) / bigpoint_constant;

}

void
Cairo_outputter::handle_metadata (SCM module)
{
  metadata ("creator", "LilyPond " + version_string (), false);
  if (!ly_is_module (module))
    return;

  for (auto const &it : metadata_keys)
    {
      std::string k = it.first;
      std::string pdf_k = "pdf" + k;
      SCM var = ly_module_lookup (module, ly_symbol2scm (pdf_k.c_str ()));
      if (!scm_is_true (var))
        var = ly_module_lookup (module, ly_symbol2scm (k.c_str ()));

      SCM val = SCM_BOOL_F;
      if (scm_is_true (var))
        val = scm_variable_ref (var);

      if (scm_is_string (val))
        metadata (k, ly_scm2string (val), true);
    }
}

SCM
Cairo_outputter::output (SCM expr)
{
  SCM head = scm_car (expr);
  expr = scm_cdr (expr);

  const int N = 9;
  SCM arg[N] = {};
  int argc = 0;
  while (scm_is_pair (expr) && argc < N)
    {
      arg[argc++] = scm_car (expr);
      expr = scm_cdr (expr);
    }
  while (argc < N)
    arg[argc++] = SCM_UNDEFINED;

  if (head == ly_symbol2scm ("circle"))
    draw_circle (arg[0], arg[1], arg[2]);
  else if (head == ly_symbol2scm ("dashed-line"))
    draw_dashed_line (arg[0], arg[1], arg[2], arg[3], arg[4], arg[5]);
  else if (head == ly_symbol2scm ("draw-line"))
    draw_line (arg[0], arg[1], arg[2], arg[3], arg[4]);
  else if (head == ly_symbol2scm ("partial-ellipse"))
    draw_partial_ellipse (arg[0], arg[1], arg[2], arg[3], arg[4], arg[5],
                          arg[6]);
  else if (head == ly_symbol2scm ("ellipse"))
    draw_ellipse (arg[0], arg[1], arg[2], arg[3]);
  else if (head == ly_symbol2scm ("glyph-string"))
    print_glyphs (arg[2], arg[4], arg[5], arg[6]);
  else if (head == ly_symbol2scm ("grob-cause"))
    grob_cause (arg[0], arg[1]);
  else if (head == ly_symbol2scm ("settranslation"))
    moveto (arg[0], arg[1]);
  else if (head == ly_symbol2scm ("named-glyph"))
    show_named_glyph (arg[0], arg[1]);
  else if (head == ly_symbol2scm ("polygon"))
    draw_polygon (arg[0], arg[1], arg[2]);
  else if (head == ly_symbol2scm ("round-filled-box"))
    draw_round_box (arg[0], arg[1], arg[2], arg[3], arg[4]);
  else if (head == ly_symbol2scm ("setcolor"))
    setrgbacolor (arg[0], arg[1], arg[2], arg[3]);
  else if (head == ly_symbol2scm ("resetcolor"))
    resetrgbacolor ();
  else if (head == ly_symbol2scm ("setrotation"))
    set_rotation (arg[0], arg[1], arg[2]);
  else if (head == ly_symbol2scm ("resetrotation"))
    reset_rotation ();
  else if (head == ly_symbol2scm ("url-link"))
    url_link (arg[0], arg[1], arg[2]);
  else if (head == ly_symbol2scm ("page-link"))
    page_link (arg[0], arg[1], arg[2]);
  else if (head == ly_symbol2scm ("path"))
    path (arg[0], arg[1], arg[2], arg[3], arg[4]);
  else if (head == ly_symbol2scm ("setscale"))
    set_scale (arg[0], arg[1]);
  else if (head == ly_symbol2scm ("resetscale"))
    reset_scale ();
  else if (head == ly_symbol2scm ("utf-8-string"))
    return SCM_BOOL_F;

  return SCM_UNSPECIFIED;
}

void
output_cairo_format (Cairo_output_format format, SCM basename, SCM stencils,
                     SCM header, Output_def *paper)
{
  Cpu_timer timer;
  Cairo_outputter outputter (format, ly_scm2string (basename), paper);

  int page = 1;
  for (SCM p = stencils; scm_is_pair (p); p = scm_cdr (p), page++)
    {
      const Stencil *stencil = unsmob<const Stencil> (scm_car (p));
      if (single_page_format (format) || page == 1)
        {
          outputter.create_surface (stencil, page);
          if (page == 1)
            outputter.handle_metadata (header);
        }
      interpret_stencil_expression (stencil->expr (), &outputter,
                                    Offset (0, 0));
      outputter.finish_page ();
    }

  outputter.close ();

  debug_output (
    String_convert::form_string ("cairo output: %f seconds", timer.read ()));
}

#endif // CAIRO_BACKEND

LY_DEFINE (ly_output_cairo, "ly:output-cairo", 4, 0, 0,
           (SCM basename, SCM stencils, SCM header, SCM paper),
           "dump book through cairo backend")
{
  (void) basename;
  (void) stencils;
  (void) header;
  (void) paper;

#if CAIRO_BACKEND
  auto *const odef = LY_ASSERT_SMOB (Output_def, paper, 4);
  for (auto const &fmt : string_split (output_format_global, ','))
    {
      Cairo_output_format f = parse_format (fmt);
      if (f == UNKNOWN)
        warning (_f ("unknown output format %s", fmt.c_str ()));
      else
        output_cairo_format (f, basename, stencils, header, odef);
    }
#else
  error ("compiled without CAIRO_BACKEND");
#endif // CAIRO_BACKEND
  return SCM_UNSPECIFIED;
}
