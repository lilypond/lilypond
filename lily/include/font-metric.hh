/*   
  font-metric.hh -- declare Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FONT_METRIC_HH
#define FONT_METRIC_HH

#include "box.hh"
/*
  sigh.  

  signature -> Internal compiler error
*/

struct Character_metric
{
  virtual Box dimensions () const=0;
  virtual ~Character_metric () {}
};

struct Font_metric
{
  virtual Character_metric const *get_char (int ascii, bool warn) const=0;
  virtual ~Font_metric () {}
};




#endif /* FONT_METRIC_HH */

