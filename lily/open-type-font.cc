/*
  open-type-font.cc --  implement Open_type_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#if 0
#include "warn.hh"
#include "open-type-font.hh"

SCM
Open_type_font::make_otf (String str)
{
  Open_type_font * otf = new Open_type_font;
  int error_code = FT_New_Face( freetype2_library, str.to_str0(),
		       0, &(otf->face_));

  //  int code = FT_Set_Charmap (otf->face_, );   

  
  if (error_code == FT_Err_Unknown_File_Format)
    {
      error("Unsupported font format");
    }
  else if (error_code)
    {
      error ("Unknown error reading font file.");
    }

  return otf->self_scm ();
}

Open_type_font::~Open_type_font()
{
  FT_Done_Face (face_);
}


Box
Open_type_font::get_indexed_char (int signed_idx)
{
  FT_UInt idx = signed_idx;
  int code = 
    FT_Load_Glyph (face_,
		   idx,
		   FT_LOAD_NO_SCALE);

  FT_Glyph_Metrics m = face->glyph->metrics;
  Box b (Interval (0, m->width) - m->horiBearingX,
	 Interval (0, m->height) - m->horiBearingY);
  
  return b;
}

int
Open_type_font::name_to_index (String nm)
{
  
}


Real
Open_type_font::get_design_size () const
{
  return 20.0;
}
#endif
