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
#include "dictionary.hh"
#include "font-metric.hh"

struct Adobe_font_char_metric : Character_metric {
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

  Box dimensions () const;
};

struct Adobe_font_metric : Font_metric {
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
  Array<int> ascii_to_metric_idx_;
  Dictionary<int> name_to_metric_dict_;
  
  Adobe_font_char_metric const &find_char (String name, bool warn=true) const;
  Adobe_font_char_metric const &find_ascii (int ascii,bool warn) const;
  String str () const;
  Adobe_font_metric ();
  void read_char_metrics (Data_file &input);


  Character_metric *get_char (int, bool) const;
};

Adobe_font_metric read_afm_file (String fn);



#endif /* AFM_HH */

