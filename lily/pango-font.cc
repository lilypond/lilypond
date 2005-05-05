/*
  pango-font.cc -- implement Pango_font

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#define PANGO_ENABLE_BACKEND // ugh, why necessary?

#include <pango/pangoft2.h>

#include "main.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "pango-font.hh"
#include "warn.hh"

#if HAVE_PANGO_FT2
#include "stencil.hh"

Pango_font::Pango_font (PangoFT2FontMap *fontmap,
			Direction dir,
			PangoFontDescription *description,
			Real output_scale)
{
  (void) fontmap;
  physical_font_tab_ = scm_c_make_hash_table (11);
  PangoDirection pango_dir = (dir == RIGHT)
    ? PANGO_DIRECTION_LTR
    : PANGO_DIRECTION_RTL;
  context_
    = pango_ft2_get_context (PANGO_RESOLUTION, PANGO_RESOLUTION);
  //  context_ = pango_ft2_font_map_create_context (fontmap);

  pango_description_ = pango_font_description_copy (description);
  attribute_list_ = pango_attr_list_new ();

  /*
    urgh. I don't understand this. Why isn't this 1/(scale *
    resolution * output_scale)

    --hwn
  */
  scale_ = INCH_TO_BP / (Real (PANGO_SCALE) * Real (PANGO_RESOLUTION) * output_scale);

  /*
    ugh. Should make this configurable.
  */
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
Pango_font::register_font_file (String filename, String ps_name)
{
  scm_hash_set_x (physical_font_tab_,
		  scm_makfrom0str (ps_name.to_str0 ()),
		  scm_makfrom0str (filename.to_str0 ()));
}

void
Pango_font::derived_mark () const
{
  scm_gc_mark (physical_font_tab_);
}

Stencil
Pango_font::pango_item_string_stencil (PangoItem *item, String str, Real dx) const
{
  const int GLYPH_NAME_LEN = 256;
  char glyph_name[GLYPH_NAME_LEN];
  PangoAnalysis *pa = &(item->analysis);
  PangoGlyphString *pgs = pango_glyph_string_new ();

  pango_shape (str.to_str0 () + item->offset,
	       item->length, pa, pgs);

  PangoRectangle logical_rect;
  PangoRectangle ink_rect;
  pango_glyph_string_extents (pgs, pa->font, &ink_rect, &logical_rect);

  PangoFcFont *fcfont = G_TYPE_CHECK_INSTANCE_CAST (pa->font,
						    PANGO_TYPE_FC_FONT,
						    PangoFcFont);

  FT_Face ftface = pango_fc_font_lock_face (fcfont);
  Box b (Interval (PANGO_LBEARING (ink_rect),
		   PANGO_RBEARING (ink_rect)),
	 Interval (-PANGO_DESCENT (ink_rect),
		   PANGO_ASCENT (ink_rect)));

  b.scale (scale_);

  SCM glyph_exprs = SCM_EOL;
  SCM *tail = &glyph_exprs;
  for (int i = 0; i < pgs->num_glyphs; i++)
    {
      PangoGlyphInfo *pgi = pgs->glyphs + i;

      PangoGlyph pg = pgi->glyph;
      PangoGlyphGeometry ggeo = pgi->geometry;

      FT_Get_Glyph_Name (ftface, pg, glyph_name, GLYPH_NAME_LEN);
      *tail = scm_cons (scm_list_3 (scm_from_double (ggeo.x_offset * scale_ + dx),
				    scm_from_double (ggeo.y_offset * scale_),
				    scm_makfrom0str (glyph_name)),
			SCM_EOL);
      dx = 0.0;
      tail = SCM_CDRLOC (*tail);
    }

  PangoFontDescription *descr = pango_font_describe (pa->font);
  Real size = pango_font_description_get_size (descr)
    / (Real (PANGO_SCALE));

  FcPattern *fcpat = fcfont->font_pattern;
  char *filename = 0;
  FcPatternGetString (fcpat, FC_FILE, 0, (FcChar8 **) & filename);
  char const *ps_name_str0 = FT_Get_Postscript_Name (ftface);

  if (!ps_name_str0)
    warning (_f ("no PostScript font name for font `%s'", filename));

  String ps_name;
  if (!ps_name_str0
      && filename
      && (String (filename).index (".otf") >= 0
	  || String (filename).index (".cff") >= 0))
    {

      /*
	UGH: kludge a PS name for OTF/CFF fonts.

      */

      String name = filename;
      int idx = max (String (filename).index (".otf"),
		     String (filename).index (".cff"));

      name = name.left_string (idx);

      int slash_idx = name.index_last ('/');	// UGh. What's happens on windows?
      if (slash_idx >= 0)
	name = name.right_string (name.length () - slash_idx - 1);

      String initial = name.cut_string (0, 1);
      initial.to_upper ();
      name = name.nomid_string (0, 1);
      name.to_lower ();
      ps_name = initial + name;
    }
  else if (ps_name_str0)
    ps_name = ps_name_str0;

  if (ps_name.length ())
    {
      ((Pango_font *) this)->register_font_file (filename, ps_name);
      pango_fc_font_unlock_face (fcfont);
	
      SCM expr = scm_list_4 (ly_symbol2scm ("glyph-string"),
			     scm_makfrom0str (ps_name.to_str0 ()),
			     scm_from_double (size),
			     ly_quote_scm (glyph_exprs));

      return Stencil (b, expr);
    }
  else
    {
      warning (_ ("FreeType face has no PostScript font name"));
      return Stencil ();
    }
}

SCM
Pango_font::physical_font_tab () const
{
  return physical_font_tab_;
}

Stencil
Pango_font::text_stencil (String str) const
{
  GList *items = pango_itemize (context_,
				str.to_str0 (),
				0, str.length (), attribute_list_,
				NULL);

  GList *ptr = items;
  Stencil dest;
  Real x = 0.0;
  while (ptr)
    {
      PangoItem *item = (PangoItem *) ptr->data;

      Stencil item_stencil = pango_item_string_stencil (item, str, x);

      x = item_stencil.extent (X_AXIS)[RIGHT];

      dest.add_stencil (item_stencil);

      ptr = ptr->next;
    }

  /*
    UGH. Should have flags per output format signifying supported
    options.
  */
  if (output_backend_global != "ps"
      && output_backend_global != "eps")
    {
      /*
	For Pango based backends, we take a shortcut.
      */
      char *descr_string = pango_font_description_to_string (pango_description_);
      SCM exp
	= scm_list_3 (ly_symbol2scm ("utf8-string"),
		      scm_makfrom0str (descr_string),
		      scm_makfrom0str (str.to_str0 ()));

      g_free (descr_string);

      Box b (Interval (0, 0), Interval (0, 0));
      b.unite (dest.extent_box ());
      return Stencil (b, exp);
    }

#if 0
  // check extents.
  if (!dest.extent_box ()[X_AXIS].is_empty ())
    {
      Stencil frame = Lookup::frame (dest.extent_box (), 0.1, 0.1);
      Box empty;
      empty.set_empty ();
      Stencil dimless_frame (empty, frame.expr ());
      dest.add_stencil (frame);
    }
#endif

  return dest;
}

SCM
Pango_font::font_file_name () const
{
  return SCM_BOOL_F;
}

#endif
