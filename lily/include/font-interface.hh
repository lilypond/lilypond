/*   
  font-interface.hh -- declare Font_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c)  2000--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FONT_INTERFACE_HH
#define FONT_INTERFACE_HH

#include "lily-proto.hh"
#include "font-metric.hh"

struct Font_interface
{
  static SCM font_alist_chain (Grob*);
  static Font_metric * get_font (Paper_def*, SCM alist_chain);
  static Font_metric * get_default_font (Grob*);
  static bool wild_compare (SCM field_val, SCM val);
  DECLARE_SCHEME_CALLBACK (properties_to_font_name, (SCM,SCM));
  DECLARE_SCHEME_CALLBACK (get_property_alist_chain, (SCM));
  static bool has_interface (Grob*);
};

#endif /* FONT_INTERFACE_HH */
