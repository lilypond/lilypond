/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "note-head.hh"

#include <math.h>
#include <cctype>
#include <algorithm> 		//  min, max

using namespace std; 

#include "directional-element-interface.hh"
#include "staff-symbol.hh"
#include "misc.hh"
#include "dots.hh"
#include "warn.hh"
#include "font-interface.hh"
#include "music.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "output-def.hh"

/*
  clean up the mess left by ledger line handling.
*/
static Stencil
internal_print (Grob *me, String *font_char)
{
  SCM style = me->get_property ("style");
  if (!scm_is_symbol (style))
    {
      style = ly_symbol2scm ("default");
    }

  SCM log = scm_int2num (Note_head::get_balltype (me));
  SCM proc = me->get_property ("glyph-name-procedure");

  String suffix =  to_string (robust_scm2int (me->get_property ("duration-log"), 2));
  if (ly_is_procedure (proc))
    suffix = ly_scm2string (scm_call_2 (proc, log, style));
  
  Font_metric *fm = Font_interface::get_default_font (me);

  Direction stem_dir = CENTER;
  if (Grob *stem = unsmob_grob (me->get_property ("stem")))
    {
      stem_dir = get_grob_direction (stem);
      if (stem_dir == CENTER)
	programming_error ("must have stem dir for note head");
    }

  Stencil out;

  String prefix = "noteheads.";
  String idx
    = prefix + ((stem_dir == UP) ? "u" : "d") + suffix;
  out = fm->find_by_name (idx);
  if (out.is_empty ())
    {
      idx = prefix + "s" + suffix;
      out = fm->find_by_name (idx);
    }

  if (out.is_empty ())
    {
      me->warning (_f ("note head `%s' not found", idx.to_str0 ()));
    }
  else
    {
      *font_char = idx;
    }

  return out;
}

MAKE_SCHEME_CALLBACK (Note_head, print, 1);
SCM
Note_head::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  String idx;
  return internal_print (me, &idx).smobbed_copy ();
}

Real
Note_head::stem_attachment_coordinate (Grob *me, Axis a)
{
  SCM brewer = me->get_property ("print-function");
  Font_metric *fm = Font_interface::get_default_font (me);

  if (brewer == Note_head::print_proc)
    {
      String key;
      internal_print (me, &key);

      int k = fm->name_to_index (key);
      if (k >= 0)
	{
	  Box b = fm->get_indexed_char (k);
	  Offset wxwy = fm->attachment_point (key);
	  Interval v = b[a];
	  if (!v.is_empty ())
	    return 2 * (wxwy[a] - v.center ()) / v.length ();
	}
    }

  /*
    Fallback
  */
  SCM v = me->get_property ("stem-attachment-function");
  if (!ly_is_procedure (v))
    return 0.0;

  SCM result = scm_call_2 (v, me->self_scm (), scm_int2num (a));
  if (!scm_is_pair (result))
    return 0.0;

  result = (a == X_AXIS) ? scm_car (result) : scm_cdr (result);

  return robust_scm2double (result, 0);
}

int
Note_head::get_balltype (Grob *me)
{
  SCM s = me->get_property ("duration-log");
  return scm_is_number (s) ? min (scm_to_int (s), 2) : 0;
}

ADD_INTERFACE (Note_head, "note-head-interface",
	       "Note head",
	       "note-names glyph-name-procedure accidental-grob style stem-attachment-function");

