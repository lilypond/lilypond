/*
  open-type-font.hh -- declare Open_type_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef OPEN_TYPE_FONT_HH
#define OPEN_TYPE_FONT_HH

#include <map>
#include "freetype.hh"
#include "font-metric.hh"

typedef std::map<FT_UInt, FT_ULong> Index_to_charcode_map;

class Open_type_font : public Font_metric
{
  FT_Face face_; /* handle to face object */

  SCM lily_character_table_; 
  SCM lily_global_table_;
  Index_to_charcode_map index_to_charcode_map_;
  Open_type_font (FT_Face);
public:
  static SCM make_otf (String);
  virtual ~Open_type_font();
  virtual Offset attachment_point (String) const;
  virtual Box get_indexed_char (int) const;
  virtual int name_to_index (String) const;
  virtual unsigned index_to_charcode (int) const;
  virtual void derived_mark () const;
#if 0
  virtual int count () const;
  virtual int index_to_ascii (int) const;
  virtual Box get_ascii_char (int) const;
  virtual Offset get_indexed_wxwy (int) const;
#endif
  virtual Real design_size () const;
};


#endif /* OPEN_TYPE_FONT_HH */

