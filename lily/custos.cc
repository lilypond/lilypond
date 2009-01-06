/*
  custos.cc -- implement Custos

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Juergen Reuter <reuter@ipd.uka.de>
*/

/* TODO:

- do not show if a clef change immediately follows in the next line

- decide: do or do not print custos if the next line starts with a rest
*/

#include <cstdio>
#include <cmath> // rint
using namespace std;

#include "custos.hh"
#include "direction.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Custos, print, 1);
SCM
Custos::print (SCM smob)
{
  Item *me = (Item *)unsmob_grob (smob);

  SCM scm_style = me->get_property ("style");
  string style;
  if (scm_is_symbol (scm_style))
    style = ly_symbol2string (scm_style);
  else
    style = "mensural";

  /*
   * Shall we use a common custos font character regardless if on
   * staffline or not, or shall we use individual font characters
   * for both cases?
   */
  bool adjust = true;

  int neutral_pos = robust_scm2int (me->get_property ("neutral-position"), 0);
  Direction neutral_direction
    = to_dir (me->get_property ("neutral-direction"));

  int pos = Staff_symbol_referencer::get_rounded_position (me);
  int sz = Staff_symbol_referencer::line_count (me) - 1;

  string font_char = "custodes." + style + ".";
  if (pos < neutral_pos)
    font_char += "u";
  else if (pos > neutral_pos)
    font_char += "d";
  else if (neutral_direction == UP)
    font_char += "u";
  else if (neutral_direction == DOWN)
    font_char += "d";
  else // auto direction; not yet supported -> use "d"
    font_char += "d";

  if (adjust)
    font_char += (((pos ^ sz) & 0x1) == 0) ? "1" : "0";
  else
    font_char += "2";

  Stencil stencil
    = Font_interface::get_default_font (me)->find_by_name (font_char);
  if (stencil.is_empty ())
    {
      me->warning (_f ("custos `%s' not found", font_char));
      return SCM_EOL;
    }

  return stencil.smobbed_copy ();
}

ADD_INTERFACE (Custos,
	       "A custos object.  @code{style} can have four valid values:"
	       " @code{mensural}, @code{vaticana}, @code{medicaea}, and"
	       " @code{hufnagel}.  @code{mensural} is the default style.",

	       /* properties */
	       "style "
	       "neutral-position "
	       "neutral-direction "
	       );
