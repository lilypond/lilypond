/*
  pango-font.cc -- implement Pango_font

  source file of the GNU LilyPond music typesetter

  (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#define PANGO_ENABLE_BACKEND // ugh, why necessary?
#include <pango/pangoft2.h>
#include <freetype/ftxf86.h>

#include <map>
#include <cstdio>

// Ugh.

#include "pango-font.hh"
#include "dimensions.hh"
#include "file-name.hh"
#include "international.hh"
#include "lookup.hh"		// debugging
#include "main.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "all-font-metrics.hh"
#include "program-option.hh"

#if HAVE_PANGO_FT2
#include "stencil.hh"

Pango_font::Pango_font (PangoFT2FontMap *fontmap,
			PangoFontDescription const *description,
			Real output_scale)
{
  (void) fontmap;

  physical_font_tab_ = scm_c_make_hash_table (11);
  PangoDirection pango_dir = PANGO_DIRECTION_LTR;
  context_ = pango_ft2_get_context (PANGO_RESOLUTION,
				    PANGO_RESOLUTION);

  pango_description_ = pango_font_description_copy (description);
  attribute_list_ = pango_attr_list_new ();

  // urgh. I don't understand this. Why isn't this 1/(scale *
  // resolution * output_scale)
  //
  //  --hwn
  output_scale_ = output_scale;
  scale_ = INCH_TO_BP
	   / (Real (PANGO_SCALE) * Real (PANGO_RESOLUTION) * output_scale);

  // ugh. Should make this configurable.
  pango_context_set_language (context_, pango_language_from_string ("en_US"));
  pango_context_set_base_dir (context_, pango_dir);
  pango_context_set_font_description (context_, description);
}

Pango_font::~Pango_font ()
{
  pango_font_description_free (pango_description_);
  g_object_unref (context_);
  pango_attr_list_unref (attribute_list_);
}

void
Pango_font::register_font_file (string filename,
				string ps_name)
{
  scm_hash_set_x (physical_font_tab_,
		  ly_string2scm (ps_name),
		  ly_string2scm (filename));
}

void
Pango_font::derived_mark () const
{
  scm_gc_mark (physical_font_tab_);
}

void
get_glyph_index_name (char *s,
		      FT_ULong code)
{
  sprintf (s, "glyphIndex%lX", code);
}

void
get_unicode_name (char *s,
		  FT_ULong code)
{
  if (code > 0xFFFF)
    sprintf (s, "u%lX", code);
  else
    sprintf (s, "uni%04lX", code);
}

Stencil
Pango_font::pango_item_string_stencil (PangoItem const *item,
				       string str,
				       bool tight_bbox) const
{
  const int GLYPH_NAME_LEN = 256;
  char glyph_name[GLYPH_NAME_LEN];
  PangoAnalysis const *pa = &(item->analysis);
  PangoGlyphString *pgs = pango_glyph_string_new ();

  pango_shape (str.c_str () + item->offset,
	       item->length, (PangoAnalysis*) pa, pgs);

  PangoRectangle logical_rect;
  PangoRectangle ink_rect;
  pango_glyph_string_extents (pgs, pa->font, &ink_rect, &logical_rect);

  PangoFcFont *fcfont = G_TYPE_CHECK_INSTANCE_CAST (pa->font,
						    PANGO_TYPE_FC_FONT,
						    PangoFcFont);

  FT_Face ftface = pango_fc_font_lock_face (fcfont);

  PangoRectangle const *which_rect = tight_bbox ? &ink_rect
						: &logical_rect;

  Box b (Interval (PANGO_LBEARING (logical_rect),
		   PANGO_RBEARING (logical_rect)),
	 Interval (-PANGO_DESCENT (*which_rect),
		   PANGO_ASCENT (*which_rect)));

  b.scale (scale_);
  char const *ps_name_str0 = FT_Get_Postscript_Name (ftface);
  FcPattern *fcpat = fcfont->font_pattern;
  FcChar8 *file_name_as_ptr = 0;
  FcPatternGetString (fcpat, FC_FILE, 0, &file_name_as_ptr);

  string file_name;
  if (file_name_as_ptr)
    // Normalize file name.
    file_name = File_name ((char const *)file_name_as_ptr).to_string ();

  SCM glyph_exprs = SCM_EOL;
  SCM *tail = &glyph_exprs;

  Index_to_charcode_map const *cmap = 0;
  bool has_glyph_names = ftface->face_flags & FT_FACE_FLAG_GLYPH_NAMES;
  if (!has_glyph_names)
    cmap = all_fonts_global->get_index_to_charcode_map (file_name, ftface);

  bool is_ttf = string (FT_Get_X11_Font_Format (ftface)) == "TrueType";
  bool cid_keyed = false;
  for (int i = 0; i < pgs->num_glyphs; i++)
    {
      PangoGlyphInfo *pgi = pgs->glyphs + i;

      PangoGlyph pg = pgi->glyph;
      PangoGlyphGeometry ggeo = pgi->geometry;

      glyph_name[0] = '\0';
      if (has_glyph_names)
	{
	  int errorcode = FT_Get_Glyph_Name (ftface, pg, glyph_name,
					     GLYPH_NAME_LEN);
	  if (errorcode)
	    programming_error (
	      _f ("FT_Get_Glyph_Name () error: %s",
		  freetype_error_string (errorcode).c_str ()));
	}

      SCM char_id = SCM_EOL;
      if (glyph_name[0] == '\0'
	  && cmap
	  && is_ttf
	  && cmap->find (pg) != cmap->end ())
	{
	  FT_ULong char_code = cmap->find (pg)->second;
	  get_unicode_name (glyph_name, char_code);
	}

      if (glyph_name[0] == '\0' && has_glyph_names)
	{
	  programming_error (
	    _f ("Glyph has no name, but font supports glyph naming.\n"
		"Skipping glyph U+%0X, file %s",
		pg, file_name.c_str ()));
	  continue;
	}

      if (glyph_name == string (".notdef") && is_ttf)
	glyph_name[0] = '\0';

      if (glyph_name[0] == '\0' && is_ttf)
	// Access by glyph index directly.
	get_glyph_index_name (glyph_name, pg);

      if (glyph_name[0] == '\0')
	{
	  // CID entry

	  cid_keyed = true;
	  char_id = scm_from_uint32 (pg);
	}
      else
	char_id = scm_from_locale_string (glyph_name);

      *tail = scm_cons (scm_list_4 (scm_from_double (ggeo.width * scale_),
				    scm_from_double (ggeo.x_offset * scale_),
				    scm_from_double (ggeo.y_offset * scale_),
				    char_id),
			SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  pango_glyph_string_free (pgs);
  pgs = 0;
  PangoFontDescription *descr = pango_font_describe (pa->font);
  Real size = pango_font_description_get_size (descr)
	      / (Real (PANGO_SCALE));

  if (!ps_name_str0)
    warning (_f ("no PostScript font name for font `%s'", file_name));

  string ps_name;
  if (!ps_name_str0
      && file_name != ""
      && (file_name.find (".otf") != NPOS
	  || file_name.find (".cff") != NPOS))
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
	  slash_idx ++;
	  name = name.substr (slash_idx,
			      name.length () - slash_idx);
	}

      string initial = name.substr (0, 1);
      initial = String_convert::to_upper (initial);
      name = name.substr (1, name.length () - 1);
      name = String_convert::to_lower (name);
      ps_name = initial + name;
    }
  else if (ps_name_str0)
    ps_name = ps_name_str0;

  if (ps_name.length ())
    {
      ((Pango_font *) this)->register_font_file (file_name, ps_name);
      pango_fc_font_unlock_face (fcfont);

      SCM expr = scm_list_5 (ly_symbol2scm ("glyph-string"),
			     ly_string2scm (ps_name),
			     scm_from_double (size),
			     scm_from_bool (cid_keyed),
			     ly_quote_scm (glyph_exprs));

      return Stencil (b, expr);
    }

  warning (_ ("FreeType face has no PostScript font name"));
  return Stencil ();
}

SCM
Pango_font::physical_font_tab () const
{
  return physical_font_tab_;
}

Stencil
Pango_font::word_stencil (string str) const
{
  return text_stencil (str, true);
}

Stencil
Pango_font::text_stencil (string str) const
{
  return text_stencil (str, false);
}

Stencil
Pango_font::text_stencil (string str,
			  bool tight) const
{
  GList *items
    = pango_itemize (context_,
		     str.c_str (),
		     0, str.length (), attribute_list_,
		     NULL);

  Stencil dest;

  Real last_x = 0.0;

  Direction text_dir = RIGHT;
  for (GList *p = items; p; p = p->next)
    {
      PangoItem *item = (PangoItem *) p->data;
      if (item->analysis.level == PANGO_DIRECTION_RTL)
	text_dir = LEFT;
    }

  for (GList *ptr = items; ptr; ptr = ptr->next)
    {
      PangoItem *item = (PangoItem *) ptr->data;

      Stencil item_stencil = pango_item_string_stencil (item, str, tight);

      if (text_dir == RIGHT)
	{
	  item_stencil.translate_axis (last_x, X_AXIS);
	  last_x = item_stencil.extent (X_AXIS)[RIGHT];
	}
      else if (text_dir == LEFT)
	dest.translate_axis (item_stencil.extent (X_AXIS)[RIGHT], X_AXIS);

#if 0 // Check extents.
      if (!item_stencil.extent_box ()[X_AXIS].is_empty ())
	{
	  Stencil frame = Lookup::frame (item_stencil.extent_box (), 0.1, 0.1);
	  Box empty;
	  empty.set_empty ();
	  Stencil dimless_frame (empty, frame.expr ());
	  dest.add_stencil (frame);
	}
#endif

      dest.add_stencil (item_stencil);
    }

  // UGH. Should have flags per output format signifying supported
  // options.
  string name = get_output_backend_name ();
  if (name != "ps" && name != "eps")
    {
      // For Pango based backends, we take a shortcut.
      SCM exp = scm_list_3 (ly_symbol2scm ("utf-8-string"),
			    ly_string2scm (description_string ()),
			    ly_string2scm (str));

      Box b (Interval (0, 0), Interval (0, 0));
      b.unite (dest.extent_box ());
      return Stencil (b, exp);
    }

  g_list_free (items);

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

#endif // HAVE_PANGO_FT2
