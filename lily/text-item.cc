/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
 */
#include <math.h>

#include "warn.hh"
#include "grob.hh"
#include "text-item.hh"
#include "font-interface.hh"
#include "virtual-font-metric.hh"
#include "paper-def.hh"

MAKE_SCHEME_CALLBACK (Text_item, interpret_markup, 3)
SCM
Text_item::interpret_markup (SCM paper, SCM props, SCM markup)
{
  if (gh_string_p (markup))
    {
      String str = ly_scm2string (markup);
      
      Paper_def *pap = unsmob_paper (paper);
      Font_metric *fm = select_font (pap, props);
      SCM lst = scm_list_n (ly_symbol2scm ("text"), markup, SCM_UNDEFINED);
      
      if (dynamic_cast<Virtual_font_metric*> (fm))
	/* ARGH. */
	programming_error ("Can't use virtual font for text.");
      else
	lst = fontify_atom (fm, lst);

      Box b = fm->text_dimension (str);
      return Stencil (b, lst).smobbed_copy ();
    }
  else if (gh_pair_p (markup))
    {
      SCM func = gh_car (markup);
      SCM args = gh_cdr (markup);
      if (!markup_p (markup))
	programming_error ("Markup head has no markup signature.");
      
      return scm_apply_2 (func, paper, props, args);
    }
  return SCM_EOL;
}

MAKE_SCHEME_CALLBACK (Text_item,print,1);
SCM
Text_item::print (SCM grob)
{
  Grob * me = unsmob_grob (grob);
  
  SCM t = me->get_property ("text");
  SCM chain = Font_interface::text_font_alist_chain (me);
  return interpret_markup (me->get_paper ()->self_scm (), chain, t);
}


/*
  Ugh. Duplicated from Scheme.
 */
bool
Text_item::markup_p (SCM x)
{
  return
    gh_string_p (x) ||
    (gh_pair_p (x)
     && SCM_BOOL_F != scm_object_property (gh_car (x), ly_symbol2scm ("markup-signature")));
}

ADD_INTERFACE (Text_item,"text-interface",
  "A scheme markup text, see @usermanref{Text markup}.",
  "text baseline-skip word-space");




