/*
  pango-font.cc --  implement Pango_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include <pango/pangoft2.h>

#include "pango-font.hh"
#include "stencil.hh" 

PangoStyle
symbol_to_pango_style (SCM style)
{
  PangoStyle pstyle = PANGO_STYLE_NORMAL;
  if (style == ly_symbol2scm ("italic"))
    {
      pstyle = PANGO_STYLE_NORMAL;
    }
  else if (style == ly_symbol2scm ("oblique")
	   || style == ly_symbol2scm ("slanted")
	   )
    {
      pstyle = PANGO_STYLE_OBLIQUE;
    }

  return pstyle;
}

PangoVariant
symbol_to_pango_variant (SCM variant)
{
  PangoVariant pvariant;
  if (variant == ly_symbol2scm ("caps"))
    {
      pvariant = PANGO_VARIANT_SMALL_CAPS;
    }
  return pvariant;
}


PangoWeight
symbol_to_pango_weight (SCM weight)
{
  PangoWeight pw = PANGO_WEIGHT_NORMAL;
  if (weight == ly_symbol2scm ("bold"))
    {
      pw = PANGO_WEIGHT_BOLD;
    }
  if (weight == ly_symbol2scm ("heavy"))
    {
      pw = PANGO_WEIGHT_HEAVY;
    }
  if (weight == ly_symbol2scm ("ultrabold"))
    {
      pw = PANGO_WEIGHT_ULTRABOLD;
    }
  if (weight == ly_symbol2scm ("light"))
    {
      pw = PANGO_WEIGHT_LIGHT;
    }
  if (weight == ly_symbol2scm ("ultralight"))
    {
      pw = PANGO_WEIGHT_ULTRALIGHT;
    }

  return pw;
}

PangoStretch
symbol_to_pango_stretch (SCM stretch)
{
  PangoStretch ps = PANGO_STRETCH_NORMAL;

  /*
    // TODO
    
  PANGO_STRETCH_ULTRA_CONDENSED,
  PANGO_STRETCH_EXTRA_CONDENSED,
  PANGO_STRETCH_CONDENSED,
  PANGO_STRETCH_SEMI_CONDENSED,
  
  PANGO_STRETCH_SEMI_EXPANDED,
  PANGO_STRETCH_EXPANDED,
  PANGO_STRETCH_EXTRA_EXPANDED,
  PANGO_STRETCH_ULTRA_EXPANDED
  */ 
  return ps;
}



PangoFontDescription* 
symbols_to_pango_font_description(SCM family,
				  SCM style,
				  SCM variant,
				  SCM weight,
				  SCM stretch,
				  SCM size)
{
  PangoFontDescription * description = pango_font_description_new ();

  pango_font_description_set_family (description,
				     ly_symbol2string (family).to_str0 ());
  pango_font_description_set_style (description,
				    symbol_to_pango_style (style));
  pango_font_description_set_variant (description,
				      symbol_to_pango_variant (variant));
  pango_font_description_set_weight (description,
				     symbol_to_pango_weight (weight));
  pango_font_description_set_stretch (description,
				      symbol_to_pango_stretch (stretch));
  pango_font_description_set_size (description,
				   gint (scm_to_double (size) * PANGO_SCALE));

  return description;
}

Pango_font::Pango_font (PangoFT2FontMap *fontmap,
			int resolution,
			Direction dir,
			PangoFontDescription * description)
{
  PangoDirection pango_dir = (dir==RIGHT)
    ? PANGO_DIRECTION_LTR
    : PANGO_DIRECTION_RTL;
  context_ = pango_ft2_font_map_create_context (fontmap);  
  attribute_list_= pango_attr_list_new();
  scale_ = PANGO_SCALE * resolution * 72.27;
  
  pango_context_set_language (context_, pango_language_from_string ("en_US"));
  pango_context_set_base_dir (context_, pango_dir);
  pango_context_set_font_description (context_, description);
}

Pango_font::~Pango_font ()
{
  g_object_unref (context_);
  pango_attr_list_unref (attribute_list_);
}


Stencil
Pango_font::text_stencil (String str) const
{
  GList * items = pango_itemize (context_,
				 str.to_str0 (),
				 0, str.length (), attribute_list_,
				 NULL);

  const int GLYPH_NAME_LEN = 256;
  char glyph_name[GLYPH_NAME_LEN];
  
  Box dest_extent;
  dest_extent.set_empty ();
  GList * ptr = items;
  SCM glyph_exprs = SCM_EOL;
  while (ptr)
    {
      PangoItem *item = (PangoItem*) ptr->data;
      PangoAnalysis *pa = &(item->analysis);
      PangoGlyphString *pgs = pango_glyph_string_new();
      pango_shape (str.to_str0 (), str.length (), pa, pgs);

      PangoRectangle logical_rect;
      pango_glyph_string_extents (pgs, pa->font, NULL, &logical_rect);
      
      PangoFcFont * fcfont = G_TYPE_CHECK_INSTANCE_CAST(pa->font,
							PANGO_TYPE_FC_FONT,
							PangoFcFont);
      FT_Face ftface = pango_fc_font_lock_face (fcfont);
      Box b (Interval (0, logical_rect.width),
	     Interval (0, logical_rect.height));

      b.translate (Offset (- logical_rect.x, -logical_rect.y));
      
      b.scale (scale_);
      dest_extent.unite (b);
      
      for (int i = 0; i < pgs->num_glyphs; i++)
	{
	  PangoGlyphInfo *pgi = pgs->glyphs + i;
	  
	  PangoGlyph pg = pgi->glyph;
	  PangoGlyphGeometry ggeo = pgi->geometry;
	  
	  FT_Get_Glyph_Name (ftface, pg, glyph_name, GLYPH_NAME_LEN);
	  glyph_exprs = scm_cons (scm_list_5 (ly_symbol2scm ("named-ps-glyph"),
					      scm_from_int (ggeo.x_offset),
					      scm_from_int (ggeo.y_offset),
					      scm_makfrom0str (glyph_name),
					      scm_makfrom0str (FT_Get_Postscript_Name (ftface))),
				  glyph_exprs);
	}

      ptr = ptr->next;      
    }

  Stencil dest (dest_extent,
		scm_cons (ly_symbol2scm ("combine-stencil"),
			  glyph_exprs));
  return dest;
}
