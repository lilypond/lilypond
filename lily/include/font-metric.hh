/*   
  font-metric.hh -- declare Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FONT_METRIC_HH
#define FONT_METRIC_HH

#include "box.hh"
#include "lily-guile.hh"
#include "smobs.hh"
#include "lily-proto.hh"

struct Font_metric
{
public:
  SCM description_;

  virtual Box get_char (int ascii) const;
  virtual Box text_dimension (String)  const;
  virtual Molecule find_by_name (String) const;

  DECLARE_SMOBS(Font_metric,);
private:
  Font_metric (Font_metric const&); // no copy.
protected:
  Font_metric ();
};


/*
  Perhaps junk this, and move iface to paper_def? 
 */
struct Scaled_font_metric : public Font_metric
{
  virtual Box text_dimension (String) const;
  virtual Molecule find_by_name (String) const;
  static SCM make_scaled_font_metric (Font_metric*, Real);
protected:
  Font_metric *orig_l_;
  Real magnification_f_;
  
  Scaled_font_metric (Font_metric*,Real);
};

Font_metric * unsmob_metrics (SCM s);

#endif /* FONT_METRIC_HH */

