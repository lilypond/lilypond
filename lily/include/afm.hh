/*
  afm.hh -- declare Adobe_font_metric

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef AFM_HH
#define AFM_HH

#include <map>
using namespace std;

#include "array.hh"
#include "font-metric.hh"
#include "parse-afm.hh"

class Adobe_font_metric : public Simple_font_metric
{
  DECLARE_CLASSNAME(Adobe_font_metric);
public:
  AFM_Font_info *font_info_;
  unsigned int checksum_;
  Real design_size_;

  ~Adobe_font_metric ();

  virtual int name_to_index (String) const;
  virtual int count () const;
  virtual int index_to_ascii (int) const;
  virtual Box get_ascii_char (int) const;
  virtual Box get_indexed_char (int) const;
  virtual Offset get_indexed_wxwy (int) const;
  static SCM make_afm (AFM_Font_info *, unsigned, Real);
  virtual Real design_size () const;
  virtual String font_name () const;

protected:
  AFM_CharMetricInfo const *find_char_metric (String name) const;
  AFM_CharMetricInfo const *find_ascii_metric (int) const;

  Array<int> ascii_to_metric_idx_;
  map<String, int> name_to_metric_dict_;

  Adobe_font_metric (AFM_Font_info *);
};

SCM read_afm_file (String);
Box afm_bbox_to_box (AFM_BBox);

#endif /* AFM_HH */

