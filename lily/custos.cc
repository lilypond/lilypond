/*
  custos.cc -- implement Custos

  source file of the GNU LilyPond music typesetter

 (C) 2000 Juergen Reuter <reuterj@ira.uka.de>
*/

/* TODO:

 - merge create_ledger_line () and Note_head::create_ledger_line ()
 

 - rewrite create_ledger_line () to support short and thin ledger lines

 - do not show if a clef change immediately follows in the next line

 - make custos direction control configurable

 - decide: do or do not print custos if the next line starts with a rest

*/


#include <stdio.h>
#include "staff-symbol-referencer.hh"
#include "custos.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "debug.hh"
#include "note-head.hh"
#include "item.hh"
#include "font-interface.hh"

/*
   This function is a patched and hopefully much more understandable
   rewrite of Note_head::ledger_line ().  It still has some
   bugs/limitations:
 *
 (1) The term thick/2 probably should be thick*2 (probably a bug,
   see the code below).
 *
 (2) The minimal width of the resulting ledger line equals the width
   of the noteheads-ledgerending symbol (a limitation):
 *
 (---- left ledger ending
     ----) right ledger ending
 (---) resulting ledger line (just ok)
 *
   If x_extent ("xwid" in Note_head) is less than the width of the
   ledger ending, the width of the total ledger line is even *greater*
   than the width of a ledger ending (I would call this a bug).  In
   the below code, the condition "if (x_extent.length () >
   slice_x_extent.length ())" avoids outputting the left ending in such
   cases (rather a silly workaround, but better than nothing).
 *
 (---- left ledger ending
     ----)   right ledger ending
 (-)   desired ledger line
     ------- resulting ledger line (too long)
     ----)   resulting ledger line with additional "if" (still too long)
 *
   The algorithm works properly only for a desired ledger line width
   greater than the width of the ledger ending:
 *
 (----    left ledger ending
        ----) right ledger ending
 (------) desired ledger line
 (------) resulting ledger line (ok)
 *
 * (3) The thickness of the ledger line is fixed (limitation).
 */
Molecule
Custos::create_ledger_line (Interval x_extent, Grob *me) 
{
  Molecule line;
  Molecule slice = Font_interface::get_default_font (me)->find_by_name ("noteheads-ledgerending");
  Interval slice_x_extent = slice.extent (X_AXIS);
  Interval slice_y_extent = slice.extent (Y_AXIS);

  // Create left ending of ledger line.
  Molecule left_ending = slice;
  left_ending.translate_axis (x_extent[LEFT] - slice_x_extent[LEFT], X_AXIS);
  if (x_extent.length () > slice_x_extent.length ())
    line.add_molecule (left_ending);

  // Create right ending of ledger line.
  Molecule right_ending = slice;
  right_ending.translate_axis (x_extent[RIGHT] - slice_x_extent[RIGHT],
			       X_AXIS);
  line.add_molecule (right_ending);

  // Fill out space between left and right ending of ledger line by
  // lining up a series of slices in a row between them.
  Molecule fill_out_slice = left_ending;
  Real thick = slice_y_extent.length ();
  Real delta_x = slice_x_extent.length () - thick;
  Real xpos = x_extent [LEFT] + 2*delta_x + thick/2; // TODO: check: thick*2?
  while (xpos <= x_extent[RIGHT])
    {
      fill_out_slice.translate_axis (delta_x, X_AXIS);
      line.add_molecule (fill_out_slice);
      xpos += delta_x;
    }

  return line;
}

void
Custos::add_streepjes (Grob* me,
		   int pos,
		   int interspaces,
		   Molecule* custos_p_)
{
  // TODO: This is (almost) duplicated code (see
  // Note_head::brew_molecule).  Junk me.
  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;
  int streepjes_i = abs (pos) < interspaces
    ? 0
    : (abs (pos) - interspaces) /2;
  if (streepjes_i) 
    {
      Direction dir = (Direction)sign (pos);
      Molecule ledger_line (create_ledger_line (custos_p_->extent (X_AXIS),
						me));
      ledger_line.set_empty (true);
      Real offs = (Staff_symbol_referencer::on_staffline (me))
	? 0.0
	: -dir * inter_f;
      for (int i = 0; i < streepjes_i; i++)
	{
	  Molecule streep (ledger_line);
	  streep.translate_axis (-dir * inter_f * i * 2 + offs,
				 Y_AXIS);
	  custos_p_->add_molecule (streep);
	}
    }
}

MAKE_SCHEME_CALLBACK (Custos,brew_molecule,1);
SCM
Custos::brew_molecule (SCM smob)
{
  Item *me = (Item *)unsmob_grob (smob);
  SCM scm_style = me->get_grob_property ("style");

  if (gh_symbol_p (scm_style))
    {
      String style = ly_scm2string (scm_symbol_to_string (scm_style));

      String idx = "custodes-";
      int interspaces = Staff_symbol_referencer::line_count (me)-1;

      Real pos = Staff_symbol_referencer::position_f (me);
      
      if (pos > (interspaces/2 + 1)) // TODO: make this rule configurable
	idx += "r";
      idx += style;
      Molecule molecule
	= Font_interface::get_default_font (me)->find_by_name (idx);
      if (molecule.empty_b ())
        {
	  String message = "unknown custos style: `" + style + "'";
	  warning (_ (message.ch_C ()));
	  return SCM_EOL;
	}
      else
        {
	  add_streepjes (me, (int)pos, interspaces, &molecule);
	  return  molecule.smobbed_copy ();
	}
    }
  else
    return SCM_EOL;
}

bool
Custos::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("custos-interface"));
}
