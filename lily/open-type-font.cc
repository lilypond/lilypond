/*
  open-type-font.cc --  implement Open_type_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include <map>
#include <stdio.h>

#include <freetype/tttables.h>
 
#include "warn.hh"
#include "open-type-font.hh"
#include "dimensions.hh"


const  Real point_constant = 1 PT;

FT_Byte *
load_table (char const *tag_str, FT_Face face, FT_ULong *length)
{
  FT_ULong tag = FT_MAKE_TAG(tag_str[0], tag_str[1], tag_str[2], tag_str[3]);
  
  int error_code = FT_Load_Sfnt_Table (face, tag, 0, NULL, length);
  if (!error_code)
    {
      FT_Byte*buffer = (FT_Byte*) malloc (*length);
      if (buffer == NULL)
	error ("Not enough memory");

      error_code = FT_Load_Sfnt_Table (face, tag, 0, buffer, length );
      if (error_code)
	{
	  error (_f ("Could not load %s font table", tag_str));
	}

      return buffer;
    }
  
  return 0 ;
}

SCM
load_scheme_table (char const *tag_str, FT_Face face)
{
  FT_ULong length = 0;
  FT_Byte* buffer = load_table (tag_str, face, &length);

  SCM tab = SCM_EOL;
  if (buffer)
    {
      String contents ((Byte const*)buffer, length);
      contents = "(quote (" +  contents + "))";

      SCM alist = scm_c_eval_string (contents.to_str0());
      tab = alist_to_hashq (alist);
      free (buffer);
    }
  return tab;
}
	    
Index_to_charcode_map
make_index_to_charcode_map (FT_Face face)
{
  Index_to_charcode_map m;
  FT_ULong  charcode;                                              
  FT_UInt   gindex;                                                
                                                                      
  charcode = FT_Get_First_Char( face, &gindex );                   
  while ( gindex != 0 )                                            
    {
      m[gindex] = charcode;
      charcode = FT_Get_Next_Char( face, charcode, &gindex );        
    }
  return m;
}                                                                  

SCM
Open_type_font::make_otf (String str)
{
  FT_Face face;
  int error_code = FT_New_Face(freetype2_library, str.to_str0(),
			       0, &face);
  
  if (error_code == FT_Err_Unknown_File_Format)
    {
      error("Unsupported font format");
    }
  else if (error_code)
    {
      error ("Unknown error reading font file.");
    }


  Open_type_font * otf = new Open_type_font (face);

    
  return otf->self_scm ();
}

Open_type_font::Open_type_font(FT_Face face)
{
  face_ = face;
  lily_character_table_ = SCM_EOL;
  lily_global_table_ = SCM_EOL;
  
  lily_character_table_ =  load_scheme_table ("LILC", face_);
  lily_global_table_ =  load_scheme_table ("LILY", face_);
  index_to_charcode_map_ = make_index_to_charcode_map (face_);  
}

void
Open_type_font::derived_mark () const
{
  scm_gc_mark (lily_character_table_);
  scm_gc_mark (lily_global_table_);
}

Offset
Open_type_font::attachment_point (String glyph_name) const
{
  SCM sym = ly_symbol2scm (glyph_name.to_str0 ());
  SCM entry = scm_hashq_ref (lily_character_table_, sym, SCM_BOOL_F);

  Offset o;
  if  (entry == SCM_BOOL_F)
    return o;

  SCM char_alist = entry;

  SCM att_scm =scm_cdr (scm_assq (ly_symbol2scm ("attachment"), char_alist));
  
  return ly_scm2offset (att_scm);
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
  return point_constant * scm_to_double (scm_hashq_ref (lily_global_table_, ly_symbol2scm ("staffsize"), SCM_BOOL_F));
}
