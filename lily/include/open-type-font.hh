/*
  open-type-font.hh -- declare Open_type_font

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef OPEN_TYPE_FONT_HH
#define OPEN_TYPE_FONT_HH

#include <map>
using namespace std;

#include "freetype.hh"
#include "font-metric.hh"

typedef map<FT_UInt, FT_ULong> Index_to_charcode_map;
Index_to_charcode_map make_index_to_charcode_map (FT_Face face);

class Open_type_font : public Font_metric
{
  /* handle to face object */
  FT_Face face_;

  SCM lily_subfonts_;
  SCM lily_character_table_;
  SCM lily_global_table_;
  SCM lily_index_to_bbox_table_;

  Index_to_charcode_map index_to_charcode_map_;
  Open_type_font (FT_Face);

  DECLARE_CLASSNAME(Open_type_font);
public:
  SCM get_subfonts () const;
  SCM get_global_table () const;
  SCM get_char_table () const;
  SCM glyph_list () const;
    
  String get_otf_table (String tag) const;
  static SCM make_otf (String);
  virtual String font_name () const;
  virtual ~Open_type_font ();
  virtual Offset attachment_point (String) const;
  virtual int count () const;
  virtual Box get_indexed_char (int) const;
  virtual int name_to_index (String) const;
  //virtual unsigned glyph_name_to_charcode (String) const;
  virtual unsigned index_to_charcode (int) const;
  virtual void derived_mark () const;
  virtual SCM sub_fonts () const;
  virtual Real design_size () const;
};

String get_otf_table (FT_Face face, String tag);
FT_Face open_ft_face (String str);

#endif /* OPEN_TYPE_FONT_HH */
