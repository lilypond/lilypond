
/*   
  afm.hh -- declare Adobe_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AFM_HH
#define AFM_HH

#include "string.hh"
#include "box.hh"
#include "array.hh"
#include "dictionary.hh"
#include "font-metric.hh"
#include "parse-afm.hh"

struct Adobe_font_metric : Font_metric
{
  AFM_Font_info * font_inf_;

  virtual Box get_char (int) const;
  AFM_CharMetricInfo const *find_char_metric (String name, bool warn=true) const;
  AFM_CharMetricInfo const *find_ascii_metric (int, bool warn=true) const;  

  String str () const;
  ~Adobe_font_metric ();
  static SCM make_afm (AFM_Font_info*, unsigned);

  unsigned int checksum_;
protected:
  Array<int> ascii_to_metric_idx_;
  Dictionary<int> name_to_metric_dict_;

  virtual Molecule find_by_name (String) const;

  Adobe_font_metric (AFM_Font_info*);
};

SCM read_afm_file (String fn);
Box afm_bbox_to_box (AFM_BBox bb);
  

#endif /* AFM_HH */

