/*
  custos.cc -- implement Custos

  source file of the GNU LilyPond music typesetter

 (C) 2000, 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

/* TODO:

 - do not show if a clef change immediately follows in the next line

 - decide: do or do not print custos if the next line starts with a rest

*/


#include <stdio.h>
#include "direction.hh"
#include "staff-symbol-referencer.hh"
#include "custos.hh"
#include "molecule.hh"
#include "warn.hh"
#include "note-head.hh"
#include "item.hh"
#include "font-interface.hh"
#include "math.h" // rint

MAKE_SCHEME_CALLBACK (Custos,brew_molecule,1);
SCM
Custos::brew_molecule (SCM smob)
{
  Item *me = (Item *)unsmob_grob (smob);

  SCM scm_style = me->get_grob_property ("style");
  String style;
  if (gh_symbol_p (scm_style))
    {
      style = ly_scm2string (scm_symbol_to_string (scm_style));
    }
  else
    {
      style = "mensural";
    }

  /*
   * Shall we use a common custos font character regardless if on
   * staffline or not, or shall we use individual font characters
   * for both cases?
   */
  bool adjust =
    to_boolean (me->get_grob_property ("adjust-if-on-staffline"));

  int neutral_pos;
  SCM ntr_pos = me->get_grob_property ("neutral-position");
  if (gh_number_p (ntr_pos))
    neutral_pos = gh_scm2int (ntr_pos);
  else
    neutral_pos = 0;

  Direction neutral_direction =
    to_dir (me->get_grob_property ("neutral-direction"));

  int pos = (int)rint (Staff_symbol_referencer::get_position (me));
  int sz = Staff_symbol_referencer::line_count (me)-1;

  String font_char = "custodes-" + style + "-";
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
    {
      font_char += (((pos ^ sz) & 0x1) == 0) ? "1" : "0";
    }
  else
    {
      font_char += "2";
    }

  Molecule molecule
    = Font_interface::get_default_font (me)->find_by_name (font_char);
  if (molecule.empty_b ())
    {
      me->warning (_f ("custos `%s' not found", font_char));
      return SCM_EOL;
    }
  else
    {
      // add ledger lines
      int pos = (int)rint (Staff_symbol_referencer::get_position (me));
      int interspaces = Staff_symbol_referencer::line_count (me)-1;
      if (abs (pos) - interspaces > 1)
	{
	  Molecule ledger_lines =
	    Note_head::brew_ledger_lines (me, pos, interspaces,
					  molecule.extent (X_AXIS), true);
	  molecule.add_molecule (ledger_lines);
	}
      return molecule.smobbed_copy ();
    }
}

ADD_INTERFACE (Custos, "custos-interface",
  "A custos is a staff context symbol that appears at the end of a
  staff line with monophonic musical contents (i.e. with a single
  voice).  It anticipates the pitch of the first note of the following
  line and thus helps the player or singer to manage line breaks
  during performance, thus enhancing readability of a score.

  Custodes were frequently used in music notation until the 16th
  century.  There were different appearences for different notation
  styles.  Nowadays, they have survived only in special forms of
  musical notation such as via the editio vaticana dating back to the
  beginning of the 20th century.

[TODO: add to glossary]",
  "style adjust-if-on-staffline neutral-direction neutral-position");
