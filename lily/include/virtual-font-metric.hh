
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
  SCM get_font_list () const;
  Virtual_font_metric (SCM namelist);
  virtual Real design_size () const;
  virtual int count () const;
  virtual Box get_indexed_char (int ascii) const;
  virtual Box get_ascii_char (int ascii) const;
  virtual Stencil get_indexed_char_stencil (int ascii) const;
  virtual Stencil get_ascii_char_stencil (int ascii) const;
  virtual Offset get_indexed_wxwy (int) const;
  virtual int name_to_index (String)const;
  virtual Stencil find_by_name (String) const;
protected:
  virtual void derived_mark () const;
};


#endif /* VIRTUAL_FONT_METRIC_HH */

