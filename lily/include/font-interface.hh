/*   
  font-interface.hh -- declare Font_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FONT_INTERFACE_HH
#define FONT_INTERFACE_HH

#include "lily-proto.hh"
#include "font-metric.hh"

struct Font_interface
{
  static SCM font_alist_chain (Grob*);
  static Font_metric * get_font (Grob*, SCM alist_chain);
  static Font_metric * get_default_font (Grob*);
  static SCM add_style (Grob*, SCM style, SCM alist_chain);
  static bool wild_compare (SCM field_val, SCM val);
  DECLARE_SCHEME_CALLBACK (properties_to_font_name, (SCM,SCM));
  static bool has_interface (Grob*);
};

#endif /* FONT_INTERFACE_HH */
