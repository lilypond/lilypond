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


struct Font_metric
{
  Font_metric ();
  SCM name_;
  virtual SCM description () const;
  virtual Box get_char (int ascii, bool warn) const;
  virtual ~Font_metric ();
  virtual Box text_dimension (String) const;

  DECLARE_SMOBS;
private:
  Font_metric (Font_metric const&); // no copy.
};


struct Scaled_font_metric : public Font_metric
{
  Font_metric *orig_l_;
  int magstep_i_;
  
  Scaled_font_metric (Font_metric*, int);
  virtual SCM description () const;
  virtual Box text_dimension (String) const;
};

Font_metric * unsmob_metrics (SCM s);


#endif /* FONT_METRIC_HH */

