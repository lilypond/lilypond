/*
  open-type-font.cc --  implement Open_type_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "warn.hh"
#include "open-type-font.hh"

#include <stdio.h>

#if 0

void
enumerate_glyphs (FT_Face face)
{
  FT_UInt glyph_index;
  FT_ULong char_code = FT_Get_First_Char (face, &glyph_index);
  while (gindex != 0)                                       
  {                                                                
    // ... do something with (charcode,gindex) pair ...               
    FT_Get
    charcode = FT_Get_Next_Char( face, charcode, &gindex );        
  }                                                                
}

#endif

SCM
Open_type_font::make_otf (String str)
{
  Open_type_font * otf = new Open_type_font;
  int error_code = FT_New_Face(freetype2_library, str.to_str0(),
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
  FT_String * nm_str = nm.to_str0 ();
  int idx = FT_Get_Name_Index (face_, nm_str);

  if (idx == 0)
    return -1;
  else
    return idx;
}


Real
Open_type_font::get_design_size () const
{
  return 20.0;
}
