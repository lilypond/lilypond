/*   
  afm.hh -- declare Adobe_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AFM_HH
#define AFM_HH

#include "string.hh"
#include "box.hh"
#include "array.hh"
#include "dictionary.hh"
#include "font-metric.hh"

struct Adobe_font_char_metric :  Character_metric {
  char_metric_info_;
  Adobe_font_char_metric (AFM_CharMetricInfo afm_inf);
  virtual Box dimensions () const;
};

struct Adobe_font_metric : Font_metric {
  AFM_Font_info * font_inf_;
  Array<int> ascii_to_metric_idx_;
  Dictionary<int> name_to_metric_dict_;

  Character_metric const *get_char (int, bool) const;
  Adobe_font_char_metric const &find_char (String name, bool warn=true) const;
  Adobe_font_char_metric const &find_ascii (int ascii,bool warn) const;
  String str () const;
  Adobe_font_metric (AFM_Font_info*);

};

Adobe_font_metric *read_afm_file (String fn);



#endif /* AFM_HH */

