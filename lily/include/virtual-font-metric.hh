
/*   
virtual-font-metric.hh -- declare Virtual_font_metric

source file of the GNU LilyPond music typesetter

(c) 2002--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef VIRTUAL_FONT_METRIC_HH
#define VIRTUAL_FONT_METRIC_HH

#include "font-metric.hh"

class Virtual_font_metric : public Font_metric
{
  SCM font_list_;
public:
  Virtual_font_metric (SCM namelist, Real, Paper_def*);

  virtual int count () const;
  virtual Box get_indexed_char (int ascii) const;
  virtual Box get_ascii_char (int ascii) const;
  virtual Molecule get_indexed_char_molecule (int ascii) const;
  virtual Molecule get_ascii_char_molecule (int ascii) const;
  virtual Offset get_indexed_wxwy (int) const;
  virtual int name_to_index (String)const;
  virtual Molecule find_by_name (String) const;

protected:
  virtual void derived_mark () const;
};


#endif /* VIRTUAL_FONT_METRIC_HH */

