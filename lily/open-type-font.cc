/*
  open-type-font.cc --  implement Open_type_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/
#include "warn.hh"
#include "open-type-font.hh"

SCM
Open_type_font::make_otf (String str)
{
  Open_type_font * otf = new Open_type_font;
  int error_code = FT_New_Face( freetype2_library, str.to_str0(),
		       0, &(otf->face_));
  
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
