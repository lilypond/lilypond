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
  static SCM font_alist_chain (Score_element*);
  static Font_metric * get_font (Score_element*, SCM alist_chain);
  static Font_metric * get_default_font (Score_element*);
  static SCM add_style (Score_element*, SCM style, SCM alist_chain);
};

#endif /* FONT_INTERFACE_HH */
