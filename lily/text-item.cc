/*   
  text-item.cc -- implement Text_item

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
 */
#include <math.h>

#include "warn.hh"
#include "grob.hh"
#include "text-item.hh"
#include "font-interface.hh"
#include "virtual-font-metric.hh"



MAKE_SCHEME_CALLBACK(Text_item,interpret_markup,3);
SCM
Text_item::interpret_markup (SCM grob, SCM props, SCM markup)
{
  if (gh_string_p (markup))
    {
      Grob *me = unsmob_grob (grob);
      Font_metric *fm = Font_interface::get_font (me, props);
  
      SCM list = scm_list_n (ly_symbol2scm ("text"), markup, SCM_UNDEFINED);
      
      if (dynamic_cast<Virtual_font_metric*> (fm))
	{
	  /*
	    ARGH.
	  */
	  programming_error ("Can't use virtual font for text.");
	}
      else
	list = fontify_atom (fm, list);

      Box b = fm->text_dimension (ly_scm2string (markup));
      return Molecule (b, list).smobbed_copy();
    }
  else if (gh_pair_p (markup))
    {
      SCM func = gh_car (markup);
      SCM args = gh_cdr (markup);
      if (!markup_p (markup))
	programming_error ("Markup head has no markup signature.");
      
      return scm_apply_2 (func, grob, props, args);
    }
  else
    {
      return SCM_EOL;
    }
}

MAKE_SCHEME_CALLBACK(Text_item,brew_molecule,1);
SCM
Text_item::brew_molecule (SCM grob)
{
  Grob * me = unsmob_grob (grob);

  SCM t = me->get_grob_property ("text");
  SCM chain = Font_interface::font_alist_chain (me);
  return interpret_markup (grob, chain, t);
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
  "A scheme markup text, see @ref{Markup functions}.",
  "text baseline-skip word-space");




