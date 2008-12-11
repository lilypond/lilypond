/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "note-head.hh"

#include <cmath>
#include <cctype>
#include <algorithm> 		//  min, max

using namespace std;

#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "warn.hh"
#include "grob.hh"

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

  string idx_symmetric;
  string idx_directed;
  string idx_either;
  idx_symmetric = idx_either = "noteheads.s" + suffix;
  Stencil out = fm->find_by_name (idx_symmetric);
  if (out.is_empty ())
    {
      string prefix = "noteheads.";

      Grob *stem = unsmob_grob (me->get_object ("stem"));
      Direction stem_dir = stem ? get_grob_direction (stem) : CENTER;
      
      if (stem_dir == CENTER)
	programming_error ("must have stem dir for note head");
      
      idx_directed = idx_either =
	prefix + ((stem_dir == UP) ? "u" : "d") + suffix;
      out = fm->find_by_name (idx_directed);
    }


  if (out.is_empty ())
    {
      me->warning (_f ("none of note heads `%s' or `%s' found",
		       idx_symmetric.c_str (), idx_directed.c_str ()));
      out = Stencil (Box (Interval (0, 0), Interval (0, 0)), SCM_EOL);
    }
  else
    *font_char = idx_either;

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

Offset
Note_head::get_stem_attachment (Font_metric *fm, string key)
{
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

  return att;
}

MAKE_SCHEME_CALLBACK (Note_head, calc_stem_attachment, 1);
SCM
Note_head::calc_stem_attachment (SCM smob)
{
  Grob *me  = unsmob_grob (smob);
  Font_metric *fm = Font_interface::get_default_font (me);
  string key;
  internal_print (me, &key);

  return ly_offset2scm (get_stem_attachment (fm, key));
}


ADD_INTERFACE (Note_head,
	       "Note head.",

	       /* properties */
	       "note-names "
	       "accidental-grob "
	       "glyph-name "
	       "stem-attachment "
	       "style "
	       );

