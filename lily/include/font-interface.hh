/*   
  font-interface.hh -- declare Font_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FONT_INTERFACE_HH
#define FONT_INTERFACE_HH

#include "lily-proto.hh"
#include "font-metric.hh"

struct Font_interface
{
  static SCM text_font_alist_chain (Grob*);
  static SCM music_font_alist_chain (Grob*);
  static Font_metric * get_default_font (Grob*);
  static bool has_interface (Grob*);
};

#endif /* FONT_INTERFACE_HH */
