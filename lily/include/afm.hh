/*   
  afm.hh -- declare Adobe_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AFM_HH
#define AFM_HH

#include "string.hh"
#include "box.hh"
#include "array.hh"

struct Adobe_font_char_metric {
  int C_;
  Real WX_;
  String N_;
  Box B_;
  Box &bbox();
  String &name();
  Real &width();
  int  &code ();
  
  String str () const;
  Adobe_font_char_metric ();
};

struct Adobe_font_metric {
  String  FontName_;
  String FullName_;
  String FamilyName_;
  String Weight_;
  Real ItalicAngle_;
  bool IsFixedPitch_;
  Box FontBBox_;
  Real UnderlinePosition_;
  Real UnderlineThickness_;
  String Version_;
  String Notice_;
  String EncodingScheme_;
  Array<Adobe_font_char_metric> char_metrics_;

  Adobe_font_char_metric find_char (String name, bool warn=true) const;
  String str () const;
  Adobe_font_metric ();
};

Adobe_font_metric read_afm (String fn);



#endif /* AFM_HH */

