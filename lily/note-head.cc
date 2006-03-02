/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "note-head.hh"

#include <cmath>
#include <cctype>
#include <algorithm> 		//  min, max
using namespace std;

#include "directional-element-interface.hh"
#include "dots.hh"
#include "font-interface.hh"
#include "international.hh"
#include "lookup.hh"
#include "misc.hh"
#include "music.hh"
#include "output-def.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "warn.hh"

/*
  clean up the mess left by ledger line handling.
*/
static Stencil
internal_print (Grob *me, string *font_char)
{
  SCM style = me->get_property ("style");
  if (!scm_is_symbol (style))
    style = ly_symbol2scm ("default");

  string suffix = to_string (min (robust_scm2int (me->get_property ("duration-log"), 2), 2));
  if (style != ly_symbol2scm ("default"))
    {
      SCM gn = me->get_property ("glyph-name");
      if (scm_is_string (gn))
	suffix = ly_scm2string (gn);
    }

  Font_metric *fm = Font_interface::get_default_font (me);

  string idx = "noteheads.s" + suffix;

  Stencil out = fm->find_by_name (idx);
  if (out.is_empty ())
    {
      string prefix = "noteheads.";
      Grob *stem = unsmob_grob (me->get_object ("stem"));
      Direction stem_dir = stem ? get_grob_direction (stem) : CENTER;

      if (stem_dir == CENTER)
	programming_error ("must have stem dir for note head");

      idx = prefix + ((stem_dir == UP) ? "u" : "d") + suffix;
      out = fm->find_by_name (idx);
    }

  if (out.is_empty ())
    {
      me->warning (_f ("note head `%s' not found", idx.c_str ()));
      out = Stencil (Box (Interval (0, 0), Interval (0, 0)), SCM_EOL);
    }
  else
    *font_char = idx;

  return out;
}

/*
  TODO: make stem X-parent of notehead. 
 */
MAKE_SCHEME_CALLBACK (Note_head, stem_x_shift, 1);
SCM
Note_head::stem_x_shift (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *stem = unsmob_grob (me->get_object ("stem"));
  if (stem)
    (void) stem->get_property ("positioning-done");

  return scm_from_int (0);
}

MAKE_SCHEME_CALLBACK (Note_head, print, 1);
SCM
Note_head::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  string idx;
  return internal_print (me, &idx).smobbed_copy ();
}

Real
Note_head::stem_attachment_coordinate (Grob *me, Axis a)
{
  Offset off = robust_scm2offset (me->get_property ("stem-attachment"),
				  Offset (0,0));
  
  return off [a];
}

MAKE_SCHEME_CALLBACK(Note_head, calc_stem_attachment, 1);
SCM
Note_head::calc_stem_attachment (SCM smob)
{
  Grob *me  = unsmob_grob (smob);
  Font_metric *fm = Font_interface::get_default_font (me);
  string key;
  internal_print (me, &key);

  Offset att;
  
  int k = fm->name_to_index (key);
  if (k >= 0)
    {
      Box b = fm->get_indexed_char (k);
      Offset wxwy = fm->attachment_point (key);
      for (int i = X_AXIS ; i < NO_AXES; i++)
	{
	  Axis a = Axis (i);
	  
	  Interval v = b[a];
	  if (!v.is_empty ())
	    {
	      att[a] = (2 * (wxwy[a] - v.center ()) / v.length ());
	    }
	}
    }

  return ly_offset2scm (att);
}


int
Note_head::get_balltype (Grob *me)
{
  SCM s = me->get_property ("duration-log");
  return scm_is_number (s) ? min (int (scm_to_int (s)), 2) : 0;
}

ADD_INTERFACE (Note_head, "note-head-interface",
	       "Note head",

	       /* properties */
	       "note-names "
	       "accidental-grob "
	       "glyph-name "
	       "stem-attachment "
	       "style "
	       );

