/*   
  text-item.cc -- implement Text_interface

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
#include "output-def.hh"
#include "modified-font-metric.hh"
#include "ly-module.hh"

MAKE_SCHEME_CALLBACK (Text_interface, interpret_string, 4)
SCM
Text_interface::interpret_string (SCM layout_smob,
			     SCM props, SCM input_encoding, SCM markup)
{
  Output_def *layout = unsmob_output_def (layout_smob);
  
  SCM_ASSERT_TYPE (layout, layout_smob, SCM_ARG1,
		   __FUNCTION__, "Layout definition");
  SCM_ASSERT_TYPE (scm_is_string (markup), markup, SCM_ARG3,
		   __FUNCTION__, "string");
  SCM_ASSERT_TYPE (input_encoding == SCM_EOL || scm_is_symbol (input_encoding),
		   input_encoding, SCM_ARG2, __FUNCTION__, "symbol");
  
  String str = ly_scm2string (markup);
  if (!scm_is_symbol (input_encoding))
    {
      SCM enc = layout->lookup_variable (ly_symbol2scm ("inputencoding"));
      if (scm_is_string (enc))
	input_encoding = scm_string_to_symbol (enc);
      else if (scm_is_symbol (enc))
	input_encoding = enc;
    }
  
  Font_metric *fm = select_encoded_font (layout, props, input_encoding);

  SCM lst = SCM_EOL;      
  Box b;
  if (Modified_font_metric* mf = dynamic_cast<Modified_font_metric*> (fm))
    {
      lst = scm_list_3 (ly_symbol2scm ("text"),
			mf->self_scm (),
			markup);
	
      b = mf->text_dimension (str);
    }
  else
    {
      /* ARGH. */
      programming_error ("Must have Modified_font_metric for text.");
      scm_display (fm->description_, scm_current_error_port ());
    }
      
  return Stencil (b, lst).smobbed_copy ();
}


MAKE_SCHEME_CALLBACK (Text_interface, interpret_markup, 3)
SCM
Text_interface::interpret_markup (SCM layout_smob, SCM props, SCM markup)
{
  if (scm_is_string (markup))
    return interpret_string (layout_smob, props, SCM_EOL, markup);
  else if (scm_is_pair (markup))
    {
      SCM func = scm_car (markup);
      SCM args = scm_cdr (markup);
      if (!markup_p (markup))
	programming_error ("Markup head has no markup signature.");
      
      return scm_apply_2 (func, layout_smob, props, args);
    }
  return SCM_EOL;
}

MAKE_SCHEME_CALLBACK (Text_interface,print,1);
SCM
Text_interface::print (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  
  SCM t = me->get_property ("text");
  SCM chain = Font_interface::text_font_alist_chain (me);
  return interpret_markup (me->get_layout ()->self_scm (), chain, t);
}

/* Ugh. Duplicated from Scheme.  */
bool
Text_interface::markup_p (SCM x)
{
  return (scm_is_string (x)
	  || (scm_is_pair (x)
	      && SCM_BOOL_F
	      != scm_object_property (scm_car (x),
				      ly_symbol2scm ("markup-signature"))));
}

ADD_INTERFACE (Text_interface,"text-interface",
	       "A scheme markup text, see @usermanref{Text markup}.",
	       "text baseline-skip word-space");




