/*   
  font-interface.cc --  implement Font_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "all-font-metrics.hh"
#include "font-metric.hh"
#include "font-interface.hh"
#include "score-element.hh"
#include "paper-def.hh"

/*
  todO : split up this func, reuse in text_item? 
 */
Font_metric *
Font_interface::get_default_font (Score_element*me)
{
  Font_metric * fm =  unsmob_metrics (me->get_elt_property ("font"));
  if (fm)
    return fm;

  SCM ss = me->paper_l ()->style_sheet_;

  SCM proc = gh_cdr (scm_assoc (ly_symbol2scm ("properties-to-font"),
				ss));

  SCM fonts = gh_cdr (scm_assoc (ly_symbol2scm ("fonts"), ss));
  SCM defaults = gh_cdr (scm_assoc (ly_symbol2scm ("font-defaults"),
				    ss));

  assert (gh_procedure_p (proc));
  SCM font_name = gh_call2 (proc, fonts,
			    gh_list (me->mutable_property_alist_,
				     me->immutable_property_alist_,
				     defaults,
				     SCM_UNDEFINED));

  fm = me->paper_l ()->find_font (font_name, 1.0);
  me->set_elt_property ("font", fm->self_scm ());
  return fm;
}
