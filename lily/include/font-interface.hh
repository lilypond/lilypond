/*   
  font-interface.hh -- declare Font_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FONT_INTERFACE_HH
#define FONT_INTERFACE_HH

#include "lily-proto.hh"
#include "font-metric.hh"

struct Font_interface
{
  static Font_metric * get_default_font (Score_element*); 
};

#endif /* FONT_INTERFACE_HH */
