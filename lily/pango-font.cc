/*
  pango-font.cc --  implement Pango_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#define PANGO_ENABLE_BACKEND // ugh, why necessary?

#include <pango/pangoft2.h>

#include "pango-font.hh"

#if HAVE_PANGO_FT2
#include "stencil.hh" 


Pango_font::Pango_font (PangoFT2FontMap *fontmap,
			int resolution,
			Direction dir,
			PangoFontDescription *description)
{
  subfonts_ = SCM_EOL;
  PangoDirection pango_dir = (dir == RIGHT)
    ? PANGO_DIRECTION_LTR
    : PANGO_DIRECTION_RTL;
  context_ = pango_ft2_font_map_create_context (fontmap);  
  attribute_list_= pango_attr_list_new();
  scale_ = 1.0 / (PANGO_SCALE * resolution * 72.27);
  
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
  subfonts_ = scm_cons (//scm_cons (scm_makfrom0str (ps_name.to_str0 ()),
			scm_makfrom0str (filename.to_str0 ()),
			subfonts_);
}

void
Pango_font::derived_mark () const
{
  scm_gc_mark (subfonts_);
}

Stencil
Pango_font::text_stencil (String str) const
{
  GList *items = pango_itemize (context_,
				str.to_str0 (),
				0, str.length (), attribute_list_,
				NULL);

  const int GLYPH_NAME_LEN = 256;
  char glyph_name[GLYPH_NAME_LEN];
  
  GList *ptr = items;
  Stencil dest;  
  while (ptr)
    {

      // FIXME: factor this out
      PangoItem *item = (PangoItem*) ptr->data;
      PangoAnalysis *pa = &(item->analysis);
      PangoGlyphString *pgs = pango_glyph_string_new ();

      pango_shape (str.to_str0 (), str.length (), pa, pgs);

      PangoRectangle logical_rect;
      pango_glyph_string_extents (pgs, pa->font, NULL, &logical_rect);
      
      PangoFcFont *fcfont = G_TYPE_CHECK_INSTANCE_CAST(pa->font,
						       PANGO_TYPE_FC_FONT,
						       PangoFcFont);
      FT_Face ftface = pango_fc_font_lock_face (fcfont);
      Box b (Interval (0, logical_rect.width),
	     Interval (0, logical_rect.height));

      if (!face_)
	{
	  /* FIXME.  This obvious shortcut apparently does not work.
	     It seems there are different faces per text string and a
	     map of face_ and charcode mapping is needed.  */
	  Pango_font *barf = (Pango_font*) this;
	  barf->face_ = ftface;
	  barf->index_to_charcode_map_ = make_index_to_charcode_map (face_);
	}

      b.translate (Offset (- logical_rect.x, -logical_rect.y));
      
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

      FcPattern *fcpat = fcfont->font_pattern;
      char *filename = 0;
      FcPatternGetString(fcpat, FC_FILE, 0, (FcChar8 **) &filename);
      char const *ps_name = FT_Get_Postscript_Name (ftface);
      ((Pango_font *) this)->register_font_file (filename, ps_name);
      
      SCM expr = scm_list_4 (ly_symbol2scm ("glyph-string"),
			     self_scm (),
			     scm_makfrom0str (ps_name),
			     ly_quote_scm (glyph_exprs));

      Stencil item_stencil (b, expr);

      dest.add_stencil (item_stencil);
      
      pango_fc_font_unlock_face (fcfont);
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

int
Pango_font::name_to_index (String nm) const
{
  char *nm_str = (char*) nm.to_str0 ();
  if (int idx = FT_Get_Name_Index (face_, nm_str))
    return idx;
  return -1;
}

unsigned
Pango_font::index_to_charcode (int i) const
{
  return ((Pango_font*) this)->index_to_charcode_map_[i];
}

LY_DEFINE (ly_pango_font_p, "ly:pango-font?",
	   1, 0, 0,
	   (SCM f),
	   "Is @var{f} a pango font?")
{
  return scm_from_bool (dynamic_cast<Pango_font*> (unsmob_metrics (f)));
}


#endif

