
/*   
  afm.hh -- declare Adobe_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AFM_HH
#define AFM_HH

#include <map>

#include "string.hh"
#include "box.hh"
#include "array.hh"
#include "font-metric.hh"
#include "parse-afm.hh"

struct Adobe_font_metric : Font_metric
{
  AFM_Font_info * font_inf_;

  virtual int name_to_index (String) const;
  virtual int count () const;
  virtual Box get_ascii_char (int) const;
  virtual Box get_indexed_char (int) const;
  virtual Offset get_indexed_wxwy (int) const;
  
  AFM_CharMetricInfo const *find_char_metric (String name) const;
  AFM_CharMetricInfo const *find_ascii_metric (int) const;  

  String to_string () const;
  ~Adobe_font_metric ();
  static SCM make_afm (AFM_Font_info*, unsigned);

  unsigned int checksum_;
protected:
  Array<int> ascii_to_metric_idx_;
  std::map<String,int> name_to_metric_dict_;

  virtual Molecule find_by_name (String) const;

  Adobe_font_metric (AFM_Font_info*);
};

SCM read_afm_file (String fn);
Box afm_bbox_to_box (AFM_BBox bb);
  

#endif /* AFM_HH */

