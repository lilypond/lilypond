/*   
  font-interface.cc --  implement Font_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "all-font-metrics.hh"
#include "font-metric.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "paper-def.hh"
#include "warn.hh"


/*
  todo: split up this func, reuse in text_item? 
 */
Font_metric *
Font_interface::get_default_font (Grob*me)
{
  Font_metric * fm =  unsmob_metrics (me->get_grob_property ("font"));
  if (fm)
    return fm;

  fm = select_font (me->get_paper (),  font_alist_chain (me));
  me->set_grob_property ("font", fm->self_scm ());
  return fm;
}


LY_DEFINE(ly_font_interface_get_default_font,
	  "ly:get-default-font", 1 , 0, 0,
	  (SCM grob), "Return the default font for grob @var{gr}.")
{
  Grob * gr  = unsmob_grob (grob);
  SCM_ASSERT_TYPE(gr, grob, SCM_ARG1, __FUNCTION__, "grob");

  return Font_interface::get_default_font (gr)->self_scm ();
}


SCM
Font_interface::font_alist_chain (Grob*g)
{
  SCM defaults = g->get_paper ()->lookup_variable (ly_symbol2scm ("font-defaults"));
  
  return g->get_property_alist_chain (defaults);
}


  


ADD_INTERFACE (Font_interface, "font-interface",
	       "Any symbol that is typeset through fixed sets of glyphs (ie. fonts)",
	       "font-magnification font font-series font-shape "
	       "font-family font-name font-design-size font-relative-size");
