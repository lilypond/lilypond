/*
  pango-font.cc --  implement Pango_font

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
  physical_font_tab_ = scm_c_make_hash_table (11);
  PangoDirection pango_dir = (dir == RIGHT)
    ? PANGO_DIRECTION_LTR
    : PANGO_DIRECTION_RTL;
  context_ =
    pango_ft2_get_context (PANGO_RESOLUTION, PANGO_RESOLUTION);

  pango_description_ = pango_font_description_copy (description);
  //  context_ = pango_ft2_font_map_create_context (fontmap);  
  attribute_list_= pango_attr_list_new();

  
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
		  scm_makfrom0str (ps_name.to_str0()),
		  scm_makfrom0str (filename.to_str0()));
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
      
  PangoFcFont *fcfont = G_TYPE_CHECK_INSTANCE_CAST(pa->font,
						   PANGO_TYPE_FC_FONT,
						   PangoFcFont);
      
  FT_Face ftface = pango_fc_font_lock_face (fcfont);
  Box b (Interval (PANGO_LBEARING(ink_rect),
		   PANGO_RBEARING(ink_rect)),
	 Interval (-PANGO_DESCENT(ink_rect),
		   PANGO_ASCENT(ink_rect)));
	     
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
    /  (Real (PANGO_SCALE));
      
  FcPattern *fcpat = fcfont->font_pattern;
  char *filename = 0;
  FcPatternGetString(fcpat, FC_FILE, 0, (FcChar8 **) &filename);
  char const *ps_name = FT_Get_Postscript_Name (ftface);

  if (ps_name)
    {
      ((Pango_font *) this)->register_font_file (filename, ps_name);
      pango_fc_font_unlock_face (fcfont);
      
      SCM expr = scm_list_4 (ly_symbol2scm ("glyph-string"),
			     scm_makfrom0str (ps_name),
			     scm_from_double (size),
			     ly_quote_scm (glyph_exprs));

      return Stencil (b, expr);
    }
  else
    {
      warning (_ ("FreeType face has no PostScript font name."));      
      return Stencil();
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
      PangoItem *item = (PangoItem*) ptr->data;

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
      SCM exp
	= scm_list_3 (ly_symbol2scm ("utf8-string"),
		      scm_makfrom0str (pango_font_description_to_string (pango_description_)),
		      scm_makfrom0str (str.to_str0 ()));


      Box b (Interval (0, 0), Interval (0, 0));
      b.unite (dest.extent_box ());
      return Stencil (b, exp);
    }
  
#if 0
  // check extents.
  if (!dest.extent_box ()[X_AXIS].is_empty ())
    {
      Stencil frame = Lookup::frame (dest.extent_box(), 0.1, 0.1);
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




void test_pango()
{
  int dpi = 1200;
  
  char * font_family = "Emmentaler";
  PangoContext * pango_context =
    pango_ft2_get_context (dpi, dpi);
  PangoFontDescription *font_description;
  font_description = pango_font_description_new ();
  pango_font_description_set_family (font_description, g_strdup (font_family));
  pango_font_description_set_style (font_description, (PangoStyle) 20);
  pango_context_set_font_description (pango_context, font_description);


  PangoAttrList *attr_list = pango_attr_list_new();
  char *str = "sfz";
  GList * items = pango_itemize (pango_context, str, 0, strlen(str),
				 attr_list, NULL);
  
  
  GList * ptr = items;
  while (ptr)
    {
      PangoItem *item = (PangoItem*)ptr->data;
      printf( "off %d len %d num %d\n", item->offset, item->length, item->num_chars);
      
      PangoAnalysis paobj = item->analysis;
      PangoAnalysis * pa = &paobj;

      PangoFontDescription *descr = pango_font_describe (pa->font);
      //      assert (font_description == descr);
      printf ("font descr string '%s' fname '%s'",
	      pango_font_description_to_string (descr),
	      pango_font_description_to_filename (descr)
	      );
      
      printf ("type name %s\n", g_type_name (G_TYPE_FROM_INSTANCE(pa->font)));
      PangoFcFont * fcfont = G_TYPE_CHECK_INSTANCE_CAST(pa->font,
							PANGO_TYPE_FC_FONT,
							PangoFcFont);

      FcPattern *fcpat = fcfont->font_pattern;
      FcPatternPrint (fcpat);
      char *retval ="bla";
      
      FcPatternGetString(fcpat, FC_FILE, 0, (FcChar8 **) &retval);
      printf ("retval %s\n", retval);

      FT_Face ftface = pango_fc_font_lock_face (fcfont);

      printf ("shape %ux %s lang %ux font %ux languagae %ux\nft face %ux\n", pa->shape_engine,
	      G_OBJECT_TYPE_NAME(pa->shape_engine),
	      pa->lang_engine, pa->font, pa->language, ftface);

      PangoGlyphString *pgs = pango_glyph_string_new();
      pango_shape (str, strlen(str), pa, pgs); 

      int i;
      for (i = 0; i < pgs->num_glyphs; i++)
	{
	  PangoGlyphInfo *pgi = pgs->glyphs + i;
	  
	  PangoGlyph pg = pgi->glyph;
	  PangoGlyphGeometry ggeo = pgi->geometry;
	  
	  printf ("c %d w %d x %d y %d\n", pg, ggeo.width, ggeo.x_offset,
		  ggeo.y_offset );

	  char str[1024];
	  FT_Get_Glyph_Name (ftface, pg, str, 1024);
	  printf ("glyph %s\n", str);
	}
      printf ("\nPS name %s\n", FT_Get_Postscript_Name (ftface));

      PangoRectangle r1;
      PangoRectangle r2;
      
      pango_glyph_string_extents (pgs, pa->font, &r1, &r2);
      
      ptr = ptr->next;
      printf ("\nnext item\n");
    }

}
