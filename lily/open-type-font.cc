/*
  open-type-font.cc --  implement Open_type_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include <stdio.h>

#include "warn.hh"
#include "open-type-font.hh"
#include "dimensions.hh"

SCM
Open_type_font::make_otf (String str)
{
  Open_type_font * otf = new Open_type_font;
  int error_code = FT_New_Face(freetype2_library, str.to_str0(),
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


Box
Open_type_font::get_indexed_char (int signed_idx) const
{
  FT_UInt idx = signed_idx;
  FT_Load_Glyph (face_,
		 idx,
		 FT_LOAD_NO_SCALE);

  FT_Glyph_Metrics m = face_->glyph->metrics;
  int hb = m.horiBearingX;
  int vb = m.horiBearingY;
  Box b (Interval (-hb, m.width - hb),
	 Interval (-vb, m.height - vb));

  Real point_constant = 1 PT;
  

  b.scale (design_size() * Real (point_constant) / face_->units_per_EM);
  return b;
}

int
Open_type_font::name_to_index (String nm) const
{
  char * nm_str = (char * )nm.to_str0 ();
  int idx = FT_Get_Name_Index (face_, nm_str);

  if (idx == 0)
    return -1;
  else
    return idx;
}


Real
Open_type_font::design_size () const
{
  return 20.0;
}
