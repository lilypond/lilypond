/*
  pango-font.cc --  implement Pango_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#define PANGO_ENABLE_BACKEND // ugh, why necessary?

#include <pango/pangoft2.h>

#include "dimensions.hh"
#include "pango-font.hh"

#if HAVE_PANGO_FT2
#include "stencil.hh" 


Pango_font::Pango_font (PangoFT2FontMap *fontmap,
			Direction dir,
			PangoFontDescription *description)
{
  subfonts_ = SCM_EOL;
  PangoDirection pango_dir = (dir == RIGHT)
    ? PANGO_DIRECTION_LTR
    : PANGO_DIRECTION_RTL;
  context_ =
    pango_ft2_get_context (PANGO_DPI, PANGO_DPI);
  //  context_ = pango_ft2_font_map_create_context (fontmap);  
  attribute_list_= pango_attr_list_new();
  scale_ = inch_constant / (Real (PANGO_SCALE) * Real (PANGO_DPI));
  
  pango_context_set_language (context_, pango_language_from_string ("en_US"));
  pango_context_set_base_dir (context_, pango_dir);
  pango_context_set_font_description (context_, description);
}

Pango_font::~Pango_font ()
{
  g_object_unref (context_);
  pango_attr_list_unref (attribute_list_);
}

void
Pango_font::register_font_file (String filename, String ps_name) 
{
  subfonts_ = scm_cons (scm_makfrom0str (filename.to_str0 ()),
			subfonts_);
}

void
Pango_font::derived_mark () const
{
  scm_gc_mark (subfonts_);
}

Stencil
Pango_font::pango_item_string_stencil (PangoItem *item, String str) const
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
      
  PangoFcFont *fcfont = G_TYPE_CHECK_INSTANCE_CAST(pa->font,
						   PANGO_TYPE_FC_FONT,
						   PangoFcFont);
      
  FT_Face ftface = pango_fc_font_lock_face (fcfont);
  Box b (Interval (PANGO_LBEARING(logical_rect),
		   PANGO_RBEARING(logical_rect)),
	 Interval (-PANGO_DESCENT(logical_rect),
		   PANGO_ASCENT(logical_rect)));
	     
  b.scale (scale_);

  SCM glyph_exprs = SCM_EOL;
  SCM *tail = &glyph_exprs;
  for (int i = 0; i < pgs->num_glyphs; i++)
    {
      PangoGlyphInfo *pgi = pgs->glyphs + i;
	  
      PangoGlyph pg = pgi->glyph;
      PangoGlyphGeometry ggeo = pgi->geometry;

      FT_Get_Glyph_Name (ftface, pg, glyph_name, GLYPH_NAME_LEN);
      *tail = scm_cons (scm_list_3 (scm_from_double (ggeo.x_offset * scale_),
				    scm_from_double (ggeo.y_offset * scale_),
				    scm_makfrom0str (glyph_name)),
			SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  PangoFontDescription *descr = pango_font_describe (pa->font);
  Real size = pango_font_description_get_size (descr)
    /  (Real (PANGO_SCALE)) ;
      
  FcPattern *fcpat = fcfont->font_pattern;
  char *filename = 0;
  FcPatternGetString(fcpat, FC_FILE, 0, (FcChar8 **) &filename);
  char const *ps_name = FT_Get_Postscript_Name (ftface);
  ((Pango_font *) this)->register_font_file (filename, ps_name);
  pango_fc_font_unlock_face (fcfont);
      
  SCM expr = scm_list_4 (ly_symbol2scm ("glyph-string"),
			 scm_makfrom0str (ps_name),
			 scm_from_double (size),
			 ly_quote_scm (glyph_exprs));

  Stencil item_stencil (b, expr);
  return item_stencil;  
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
      PangoItem *item = (PangoItem*) ptr->data;

      Stencil item_stencil = pango_item_string_stencil (item, str);

      item_stencil.translate_axis (x, X_AXIS);
      x += item_stencil.extent (X_AXIS)[RIGHT];
      
      dest.add_stencil (item_stencil);
      
      ptr = ptr->next;      
    }

  return dest;
}

SCM
Pango_font::sub_fonts () const
{
  return subfonts_;
}

SCM 
Pango_font::font_file_name () const
{
  return SCM_BOOL_F;
}

LY_DEFINE (ly_pango_font_p, "ly:pango-font?",
	   1, 0, 0,
	   (SCM f),
	   "Is @var{f} a pango font?")
{
  return scm_from_bool (dynamic_cast<Pango_font*> (unsmob_metrics (f)));
}


#endif

