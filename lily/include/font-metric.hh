/*   
  font-metric.hh -- declare Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1999--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FONT_METRIC_HH
#define FONT_METRIC_HH

#include "box.hh"
#include "lily-guile.hh"
#include "smobs.hh"
#include "lily-proto.hh"
#include "string.hh"

struct Font_metric
{
public:
  SCM description_;
  String path_;
  
  virtual int count () const;
  virtual Offset get_indexed_wxwy (int) const;
  virtual Box get_indexed_char (int index) const;
  virtual Box get_ascii_char (int ascii) const;
  virtual Box text_dimension (String)  const;
  virtual int name_to_index (String) const;

  virtual Molecule find_by_name (String) const;
  virtual Molecule get_indexed_char_molecule (int k) const;
  virtual Molecule get_ascii_char_molecule (int k) const;  
  
  DECLARE_SMOBS (Font_metric,);
private:
  Font_metric (Font_metric const&); // no copy.
protected:
  virtual void derived_mark () const;

  Font_metric ();
};

DECLARE_UNSMOB(Font_metric, metrics);

#endif /* FONT_METRIC_HH */
