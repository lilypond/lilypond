/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>
#include <ctype.h>

#include "staff-symbol.hh"
#include "misc.hh"
#include "dots.hh"
#include "note-head.hh"
#include "warn.hh"
#include "font-interface.hh"
#include "stencil.hh"
#include "event.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "output-def.hh"

/*
  clean up the mess left by ledger line handling.
*/
static Stencil
internal_print (Grob *me)
{
  SCM style  = me->get_property ("style");
  if (!scm_is_symbol (style))
    {
      return Stencil ();
    }

  SCM log = scm_int2num (Note_head::get_balltype (me));
  SCM proc = me->get_property ("glyph-name-procedure");
  SCM scm_font_char = scm_call_2 (proc, log, style);
  String font_char = "noteheads-" + ly_scm2string (scm_font_char);

  Font_metric * fm = Font_interface::get_default_font (me);
  Stencil out = fm->find_by_name (font_char);
  if (out.is_empty ())
    {
      me->warning (_f ("note head `%s' not found", font_char.to_str0 ()));
    }

  return out;
}


MAKE_SCHEME_CALLBACK (Note_head,print,1);
SCM
Note_head::print (SCM smob)  
{
  Grob *me = unsmob_grob (smob);

  return internal_print (me).smobbed_copy ();
}


MAKE_SCHEME_CALLBACK (Note_head,brew_ez_stencil,1);
SCM
Note_head::brew_ez_stencil (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  int l = Note_head::get_balltype (me);

  int b = (l >= 2);

  SCM cause = me->get_property ("cause");
  SCM spitch = unsmob_music (cause)->get_property ("pitch");
  Pitch* pit =  unsmob_pitch (spitch);

  SCM idx = scm_int2num (pit->get_notename ());
  SCM names = me->get_property ("note-names");
  SCM charstr = SCM_EOL;
  if (ly_c_vector_p (names))
    charstr = scm_vector_ref (names, idx);
  else
    {
      char s[2] = "a";
      s[0] = (pit->get_notename () + 2)%7 + 'a';
      s[0] = toupper (s[0]);
      charstr = scm_makfrom0str (s);
    }
  
  SCM at = scm_list_n (ly_symbol2scm ("ez-ball"),
		       charstr,
		       scm_int2num (b),
		       scm_int2num (1-b),
		       SCM_UNDEFINED);
  Box bx (Interval (0, 1.0), Interval (-0.5, 0.5));
  Stencil m (bx, at);

  return m.smobbed_copy ();
}


Real
Note_head::stem_attachment_coordinate (Grob *me, Axis a)
{
  SCM brewer = me->get_property ("print-function");
  Font_metric * fm  = Font_interface::get_default_font (me);
  
  if (brewer == Note_head::print_proc)
    {
      SCM style  = me->get_property ("style");
      if (!scm_is_symbol (style))
	{
	  return 0.0;
	}
      
      SCM log = scm_int2num (Note_head::get_balltype (me));
      SCM proc = me->get_property ("glyph-name-procedure");
      SCM scm_font_char = scm_call_2 (proc, log, style);
      String font_char = "noteheads-" + ly_scm2string (scm_font_char);

      int k = fm->name_to_index (font_char) ;

      if (k >= 0)
	{
	  Box b = fm->get_indexed_char (k);
	  Offset wxwy = fm->get_indexed_wxwy (k);
	  Interval v = b[a];
	  if (!v.is_empty ())
	    return 2 * (wxwy[a] - v.center ()) / v.length ();
	}
    }
  
  /*
    Fallback
   */
  SCM v = me->get_property ("stem-attachment-function");
  if (!ly_c_procedure_p (v))
    return 0.0;
  
  SCM result = scm_call_2 (v, me->self_scm (), scm_int2num (a));
  if (!scm_is_pair (result))
    return 0.0;

  result = (a == X_AXIS) ? ly_car (result) : ly_cdr (result);
  
  return robust_scm2double (result,0);
}

int
Note_head::get_balltype (Grob*me) 
{
  SCM s = me->get_property ("duration-log");
  return scm_is_number (s) ? scm_to_int (s) <? 2 : 0;
}

ADD_INTERFACE (Note_head,"note-head-interface",
  "Note head",
  "note-names glyph-name-procedure accidental-grob style stem-attachment-function");

