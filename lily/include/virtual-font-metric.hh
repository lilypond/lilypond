
/*   
virtual-font-metric.hh -- declare Virtual_font_metric

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

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
  virtual Box get_char (int ascii) const;
  virtual Molecule get_char_molecule (int ascii) const;
  
  virtual Molecule find_by_name (String) const;

protected:
  virtual void derived_mark();
};


#endif /* VIRTUAL_FONT_METRIC_HH */

