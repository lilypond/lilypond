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

// The PangoFcFont definition is only visible if PANGO_ENABLE_BACKEND
// is defined.
#define PANGO_ENABLE_BACKEND
#include <pango/pangofc-font.h>
#include <pango/pangoft2.h>
#include FT_XFREE86_H
#include FT_CID_H

#include "pango-font.hh"
#include "dimensions.hh"
#include "file-name.hh"
#include "international.hh"
#include "lookup.hh" // debugging
#include "ly-module.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "all-font-metrics.hh"
#include "program-option.hh"
#include "open-type-font.hh"

#include "stencil.hh"

using std::string;

// RAII for loading a PangoFont from PangoContext and casting to PangoFcFont.
class PangoFont_accessor
{
  PangoFont *pango_font_;

public:
  PangoFont_accessor (PangoContext *context,
                      PangoFontDescription *pango_description)
  {
    pango_font_ = pango_context_load_font (context, pango_description);
  }

  ~PangoFont_accessor () { g_object_unref (pango_font_); }

  operator PangoFont * () { return pango_font_; }
  operator PangoFcFont * () { return PANGO_FC_FONT (pango_font_); }
};

// RAII for extracting FT_Face from PangoFcFont
class FTFace_accessor
{
  PangoFcFont *pango_font_;
  FT_Face face_;

public:
  operator FT_Face () { return face_; }
  FT_Face operator->() { return face_; }
  FTFace_accessor (PangoFcFont *pango_font)
  {
    pango_font_ = pango_font;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    // This is deprecated in Pango 1.44, but we still support 1.36.
    face_ = pango_fc_font_lock_face (pango_font);
#pragma GCC diagnostic pop
  }

  ~FTFace_accessor ()
  {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    // Idem.
    pango_fc_font_unlock_face (pango_font_);
#pragma GCC diagnostic pop
  }
};

Preinit_Pango_font::Preinit_Pango_font ()
{
  physical_font_tab_ = SCM_EOL;
}

Pango_font::Pango_font (PangoFT2FontMap *fontmap,
                        PangoFontDescription const *description,
                        Real output_scale)
{
  physical_font_tab_ = scm_c_make_hash_table (11);
  PangoDirection pango_dir = PANGO_DIRECTION_LTR;
  context_ = pango_context_new ();
  pango_context_set_font_map (context_, PANGO_FONT_MAP (fontmap));

  pango_description_ = pango_font_description_copy (description);

  // urgh. I don't understand this. Why isn't this 1/(scale *
  // resolution * output_scale)
  //
  //  --hwn
  scale_ = INCH_TO_BP
           / (static_cast<Real> (PANGO_SCALE)
              * static_cast<Real> (PANGO_RESOLUTION) * output_scale);

  // ugh. Should make this configurable.
  pango_context_set_language (context_, pango_language_from_string ("en_US"));
  pango_context_set_base_dir (context_, pango_dir);
  pango_context_set_font_description (context_, description);
}

Pango_font::~Pango_font ()
{
  pango_font_description_free (pango_description_);
  g_object_unref (context_);
}

void
Pango_font::register_font_file (const string &filename, const string &ps_name,
                                int face_index)
{
  scm_hash_set_x (physical_font_tab_, ly_string2scm (ps_name),
                  ly_list (ly_string2scm (filename), to_scm (face_index)));
}

size_t
Pango_font::name_to_index (string nm) const
{
  PangoFont_accessor fcfont (context_, pango_description_);
  FTFace_accessor face (fcfont);
  FT_UInt idx = FT_Get_Name_Index (face, const_cast<char *> (nm.c_str ()));
  return (idx != 0) ? idx : GLYPH_INDEX_INVALID;
}

static PangoGlyph
glyph_index_to_pango (size_t index)
{
  if (index != GLYPH_INDEX_INVALID)
    {
      // TODO: size_t can be wider than PangoGlyph.  Using a cast to silence
      // warnings is probably OK in practice but is still a little concerning.
      // Consider changing the type used for indices or mapping out-of-range
      // indices to PANGO_GLYPH_INVALID_INPUT.
      return static_cast<PangoGlyph> (index);
    }

  return PANGO_GLYPH_INVALID_INPUT;
}

void
Pango_font::derived_mark () const
{
  scm_gc_mark (physical_font_tab_);
}

void
get_glyph_index_name (char *s, FT_ULong code)
{
  sprintf (s, "glyphIndex%lX", code);
}

void
get_unicode_name (char *s, FT_ULong code)
{
  if (code > 0xFFFF)
    sprintf (s, "u%lX", code);
  else
    sprintf (s, "uni%04lX", code);
}

Box
Pango_font::get_unscaled_indexed_char_dimensions (size_t signed_idx) const
{
  PangoFont_accessor fcfont (context_, pango_description_);
  FTFace_accessor face (fcfont);
  Box b = ly_FT_get_unscaled_indexed_char_dimensions (face, signed_idx);
  return b;
}

Box
Pango_font::get_scaled_indexed_char_dimensions (size_t signed_idx) const
{
  PangoFont_accessor font (context_, pango_description_);
  PangoRectangle logical_rect;
  PangoRectangle ink_rect;
  PangoGlyph glyph = glyph_index_to_pango (signed_idx);
  pango_font_get_glyph_extents (font, glyph, &ink_rect, &logical_rect);
  Box out (Interval (PANGO_LBEARING (ink_rect), PANGO_RBEARING (ink_rect)),
           Interval (-PANGO_DESCENT (ink_rect), PANGO_ASCENT (ink_rect)));
  out.scale (scale_);
  return out;
}

Box
Pango_font::get_glyph_outline_bbox (size_t signed_idx) const
{
  PangoFont_accessor fcfont (context_, pango_description_);
  FTFace_accessor face (fcfont);
  Box b = ly_FT_get_glyph_outline_bbox (face, signed_idx);
  return b;
}

void
Pango_font::add_outline_to_skyline (Lazy_skyline_pair *lazy,
                                    Transform const &transform,
                                    size_t signed_idx) const
{
  PangoFont_accessor fcfont (context_, pango_description_);
  FTFace_accessor face (fcfont);
  ly_FT_add_outline_to_skyline (lazy, transform, face, signed_idx);
}

SCM
Pango_font::get_glyph_desc (PangoGlyphInfo const &pgi,
                            Box const &scaled_glyph_extent,
                            std::string const &file_name, FT_Face ftface,
                            bool *cid_keyed) const
{
  PangoGlyph pg = pgi.glyph;
  PangoGlyphGeometry ggeo = pgi.geometry;
  bool is_ttf = string (FT_Get_X11_Font_Format (ftface)) == "TrueType";
  bool has_glyph_names = ftface->face_flags & FT_FACE_FLAG_GLYPH_NAMES;

  const int GLYPH_NAME_LEN = 256;
  char glyph_name[GLYPH_NAME_LEN];

  /*
    Zero-width characters are valid Unicode characters,
    but glyph lookups need to be skipped.
  */
  if (!(pg ^ PANGO_GLYPH_EMPTY))
    return SCM_BOOL_F;

  if (pg & PANGO_GLYPH_UNKNOWN_FLAG)
    {
      warning (_f ("no glyph for character U+%04X in font `%s'",
                   pg & ~PANGO_GLYPH_UNKNOWN_FLAG, file_name.c_str ()));
      return SCM_BOOL_F;
    }

  glyph_name[0] = '\0';
  if (has_glyph_names)
    {
      FT_Error errorcode
        = FT_Get_Glyph_Name (ftface, pg, glyph_name, GLYPH_NAME_LEN);
      if (errorcode)
        programming_error (_f ("FT_Get_Glyph_Name () error: %s",
                               freetype_error_string (errorcode).c_str ()));
    }

  SCM char_id = SCM_EOL;
  if (glyph_name[0] == '\0' && is_ttf)
    {
      Index_to_charcode_map const *cmap = 0;
      if (!has_glyph_names)
        cmap = all_fonts_global->get_index_to_charcode_map (
          file_name, static_cast<int> (ftface->face_index), ftface);

      if (cmap && cmap->find (pg) != cmap->end ())
        {
          FT_ULong char_code = cmap->find (pg)->second;
          get_unicode_name (glyph_name, char_code);
        }
    }

  if (glyph_name[0] == '\0' && has_glyph_names)
    {
      programming_error (
        _f ("Glyph has no name, but font supports glyph naming.\n"
            "Skipping glyph U+%04X, file %s",
            pg, file_name.c_str ()));
      return SCM_BOOL_F;
    }

  if (glyph_name == string (".notdef") && is_ttf)
    glyph_name[0] = '\0';

  if (glyph_name[0] == '\0' && is_ttf)
    // Access by glyph index directly.
    get_glyph_index_name (glyph_name, pg);

  if (glyph_name[0] == '\0')
    {
      *cid_keyed = true;
      // We extract the raw CFF from the SFNT and write it to the PS
      // output.  If this font is CID-keyed we have to map the SFNT
      // `char_id` to the raw CFF's glyph ID (which corresponds to the
      // CID key).  Note that `FT_Get_CID_From_Glyph_Index` is a no-op
      // otherwise.
      FT_UInt cid_id;
      FT_Error errorcode = FT_Get_CID_From_Glyph_Index (ftface, pg, &cid_id);
      if (errorcode)
        {
          programming_error (_f ("FT_Get_CID_From_Glyph_Index () error: %s\n"
                                 "Skipping glyph U+%04X, file %s",
                                 freetype_error_string (errorcode).c_str (), pg,
                                 file_name.c_str ()));
          return SCM_BOOL_F;
        }
      char_id = scm_from_uint32 (cid_id);
    }
  else
    char_id = scm_from_utf8_string (glyph_name);

  return ly_list (to_scm (scaled_glyph_extent[X_AXIS][RIGHT]
                          - scaled_glyph_extent[X_AXIS][LEFT]),
                  scm_cons (to_scm (scaled_glyph_extent[Y_AXIS][DOWN]),
                            to_scm (scaled_glyph_extent[Y_AXIS][UP])),
                  to_scm (ggeo.x_offset * scale_),
                  to_scm (-ggeo.y_offset * scale_), char_id);
}

Stencil
Pango_font::pango_item_string_stencil (PangoGlyphItem const *glyph_item,
                                       std::string const &text) const
{
  PangoAnalysis const *pa = &(glyph_item->item->analysis);
  PangoGlyphString *pgs = glyph_item->glyphs;

  PangoRectangle logical_rect;
  PangoRectangle ink_rect;
  pango_glyph_string_extents (pgs, pa->font, &ink_rect, &logical_rect);

  PangoFcFont *fcfont = PANGO_FC_FONT (pa->font);
  FTFace_accessor ftface (fcfont);

  Box string_extent (
    Interval (PANGO_LBEARING (logical_rect), PANGO_RBEARING (logical_rect)),
    Interval (-PANGO_DESCENT (ink_rect), PANGO_ASCENT (ink_rect)));

  string_extent.scale (scale_);

  const string ps_name_str0 = get_postscript_name (ftface);
  FcPattern *fcpat = fcfont->font_pattern;

  FcChar8 *file_name_as_ptr = 0;
  FcPatternGetString (fcpat, FC_FILE, 0, &file_name_as_ptr);

  string file_name;
  if (file_name_as_ptr)
    {
      // Normalize file name.
      auto cstr = reinterpret_cast<char const *> (file_name_as_ptr);
      file_name = File_name (cstr).to_string ();
    }

  PangoGlyphItemIter cluster_iter;
  SCM clusters = SCM_EOL;
  SCM *clusters_tail = &clusters;
  for (gboolean have_cluster = pango_glyph_item_iter_init_start (
         &cluster_iter, const_cast<PangoGlyphItem *> (glyph_item),
         text.c_str ());
       have_cluster;
       have_cluster = pango_glyph_item_iter_next_cluster (&cluster_iter))
    {
      int idx_delta = cluster_iter.end_index - cluster_iter.start_index;
      int gl_delta = cluster_iter.end_glyph - cluster_iter.start_glyph;

      *clusters_tail
        = scm_cons (scm_cons (to_scm (idx_delta), to_scm (gl_delta)), SCM_EOL);
      clusters_tail = SCM_CDRLOC (*clusters_tail);
    }

  SCM glyph_exprs = SCM_EOL;
  SCM *tail = &glyph_exprs;
  bool cid_keyed = false;

  for (int i = 0; i < pgs->num_glyphs; i++)
    {
      PangoRectangle logical_sub_rect;
      PangoRectangle ink_sub_rect;

      pango_glyph_string_extents_range (pgs, i, i + 1, pa->font, &ink_sub_rect,
                                        &logical_sub_rect);
      Box b_sub (
        Interval (PANGO_LBEARING (logical_sub_rect),
                  PANGO_RBEARING (logical_sub_rect)),
        Interval (-PANGO_DESCENT (ink_sub_rect), PANGO_ASCENT (ink_sub_rect)));

      b_sub.scale (scale_);

      SCM glyph_desc
        = get_glyph_desc (pgs->glyphs[i], b_sub, file_name, ftface, &cid_keyed);

      if (scm_is_false (glyph_desc))
        {
          // If we skip a glyph, the UTF-8 text, glyph list and cluster
          // list will go out of sync. In this case, give up on
          // copy&paste from PDFs.
          clusters = SCM_BOOL_F;
          continue;
        }

      *tail = scm_cons (glyph_desc, SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  if (!scm_is_pair (glyph_exprs))
    return Stencil (string_extent, SCM_EOL);

  PangoFontDescription *descr = pango_font_describe (pa->font);
  Real size = pango_font_description_get_size (descr)
              / (static_cast<Real> (PANGO_SCALE));
  pango_font_description_free (descr);

  if (ps_name_str0.empty ())
    warning (_f ("no PostScript font name for font `%s'", file_name));

  string ps_name;
  if (ps_name_str0.empty () && file_name != ""
      && (file_name.find (".otf") != NPOS || file_name.find (".cff") != NPOS))
    {
      // UGH: kludge a PS name for OTF/CFF fonts.
      string name = file_name;
      ssize idx = file_name.find (".otf");
      if (idx == NPOS)
        idx = file_name.find (".cff");

      name = name.substr (0, idx);

      ssize slash_idx = name.rfind ('/');
      if (slash_idx != NPOS)
        {
          slash_idx++;
          name = name.substr (slash_idx, name.length () - slash_idx);
        }

      string initial = name.substr (0, 1);
      initial = String_convert::to_upper (initial);
      name = name.substr (1, name.length () - 1);
      name = String_convert::to_lower (name);
      ps_name = initial + name;
    }
  else if (!ps_name_str0.empty ())
    ps_name = ps_name_str0;

  if (ps_name.length ())
    {
      int face_index = static_cast<int> (ftface->face_index);
      // Hm.  Is register_font_file() const or is it not?
      {
        auto me = const_cast<Pango_font *> (this);
        me->register_font_file (file_name, ps_name, face_index);
      }

      std::string substr
        = text.substr (glyph_item->item->offset, glyph_item->item->length);
      SCM expr
        = ly_list (ly_symbol2scm ("glyph-string"), self_scm (),
                   ly_string2scm (ps_name), to_scm (size), to_scm (cid_keyed),
                   glyph_exprs, ly_string2scm (file_name), to_scm (face_index),
                   ly_string2scm (substr), clusters);

      return Stencil (string_extent, expr);
    }

  warning (_ ("FreeType face has no PostScript font name"));
  return Stencil ();
}

SCM
Pango_font::physical_font_tab () const
{
  return physical_font_tab_;
}

extern bool music_strings_to_paths;

Stencil
Pango_font::text_stencil (Output_def * /* state */, const string &str,
                          bool music_string, const string &features_str) const
{
  /*
    The text assigned to a PangoLayout is automatically divided
    into sections and reordered according to the Unicode
    Bidirectional Algorithm, if necessary.
  */
  PangoLayout *layout = pango_layout_new (context_);

  if (!features_str.empty ())
    {
      PangoAttrList *list = pango_attr_list_new ();
      PangoAttribute *features_attr
        = pango_attr_font_features_new (features_str.c_str ());
      pango_attr_list_insert (list, features_attr);
      pango_layout_set_attributes (layout, list);
      pango_attr_list_unref (list);
    }

  pango_layout_set_text (layout, str.c_str (), static_cast<int> (str.size ()));
  GSList *lines = pango_layout_get_lines (layout);

  Stencil dest;
  Real last_x = 0.0;

  for (GSList *l = lines; l; l = l->next)
    {
      /* Maybe the individual lines should be offset by Y? It's hard
         to tell, as we we never encounter this situation in the
         regtest. */
      auto *const line = static_cast<PangoLayoutLine *> (l->data);
      GSList *layout_runs = line->runs;
      std::string line_text = str.substr (line->start_index, line->length);
      for (GSList *p = layout_runs; p; p = p->next)
        {
          auto *const item = static_cast<PangoGlyphItem *> (p->data);
          Stencil item_stencil = pango_item_string_stencil (item, line_text);

          item_stencil.translate_axis (last_x, X_AXIS);
          last_x = item_stencil.extent (X_AXIS)[RIGHT];
          dest.add_stencil (item_stencil);
        }
    }

  g_object_unref (layout);

  if (!scm_is_null (dest.expr ()) && (!music_string || !music_strings_to_paths))
    {
      // Encapsulate to allow a short-cut for backends that also use
      // Pango for rendering.
      SCM exp = ly_list (ly_symbol2scm ("utf-8-string"),
                         ly_string2scm (description_string ()),
                         ly_string2scm (str), dest.expr ());
      dest = Stencil (dest.extent_box (), exp);
    }
  return dest;
}

string
Pango_font::description_string () const
{
  char *descr_string = pango_font_description_to_string (pango_description_);
  string s (descr_string);
  g_free (descr_string);
  return s;
}

SCM
Pango_font::font_file_name () const
{
  return SCM_BOOL_F;
}
