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
#include "paper-def.hh"

/*
  Note_head contains the code for printing note heads.

  Ledger lines:

  It also contains the ledger lines, for historical reasons.  Ledger
  lines are somewhat of a PITA. In some cases, they take up no space, in
  some cases they don't:

  DO take space:

  - when ledgered notes are juxtaposed: there should be some white
   space between the ledger lines.

  - when accidentals are near: the accidentals should not be on the
  ledger lines

  [both tips by Heinz Stolba from Universal Edition].

  DO NOT take space into account:

  - for basically everything else, e.g. swapping ledgered notes on
   clustered chords, spacing between ledgered and unledgered notes.
  
  TODO: fix this. It is not feasible to have a special grob for
  ledgers, since you basically don't know if there will be ledgers,
  unless you know at interpretation phase already 1. the Y-position,
  2. the number of staff lines. It's not yet specced when both pieces
  of information are there, so for now, it is probably better to build
  special support for ledgers into the accidental and separation-item
  code.

  (Besides a separate ledger seems overkill. For what else would
  it be useful?)

*/

/*
  TODO: ledger lines are also a property of the staff. Maybe move them
  to there?
 */
Stencil
Note_head::brew_ledger_lines (Grob *me,
                              int pos,
                              int interspaces,
                              Interval x_extent,
			      Real left_shorten,
			      bool take_space)
{
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;
  int line_count = (abs (pos) < interspaces)
    ? 0
    : (abs (pos) - interspaces) / 2;
  Stencil stencil = Stencil ();


  if (line_count)
    {
      Real ledgerlinethickness =
	Staff_symbol::get_ledger_line_thickness (staff);
      Real blotdiameter = ledgerlinethickness;
      Interval y_extent =
	Interval (-0.5*(ledgerlinethickness),
		  +0.5*(ledgerlinethickness));
      Stencil proto_ledger_line =
	Lookup::round_filled_box (Box (x_extent, y_extent), blotdiameter);

      x_extent[LEFT] += left_shorten;
      Stencil proto_first_line =
	Lookup::round_filled_box (Box (x_extent, y_extent), blotdiameter);

      if (!take_space)
        {
	  proto_ledger_line.set_empty (true);
	  proto_first_line.set_empty (true);
	}
      
      Direction dir = (Direction)sign (pos);
      Real offs = (Staff_symbol_referencer::on_staffline (me, pos))
        ? 0.0
        : -dir * inter_f;
      
      for (int i = 0; i < line_count; i++)
        {
          Stencil ledger_line ((i == 0) 
				? proto_first_line
				: proto_ledger_line
				);
          ledger_line.translate_axis (-dir * inter_f * i * 2 + offs, Y_AXIS);
          stencil.add_stencil (ledger_line);
        }
    }

  return stencil;
}

Stencil
internal_print (Grob *me, bool with_ledgers)
{
  SCM style  = me->get_property ("style");
  if (!ly_symbol_p (style))
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

  int interspaces = Staff_symbol_referencer::line_count (me)-1;
  int pos = Staff_symbol_referencer::get_rounded_position (me);
  if (with_ledgers && interspaces >= 0
      && abs (pos) - interspaces > 1)
    {
      Interval ledger_size = out.extent (X_AXIS);
      ledger_size.widen ( ledger_size.length ()/4);

      Real left_shorten =0.0;
      if (Grob * g = unsmob_grob (me->get_property ("accidental-grob")))
	{
	  /*
	    make a little room for accidentals.
	  
	    TODO: this will look silly if a chord has ledger lines,
	    and only the bottom note has an accidental.
	  */

	  Grob *common = g->common_refpoint (me, X_AXIS);
	  Real d =
	    linear_combination (Drul_array<Real> (me->extent (common, X_AXIS)[LEFT],
						  g->extent (common, X_AXIS)[RIGHT]),
				
				0.5);

	  left_shorten =  (-ledger_size[LEFT] + d) >?  0 ;

	  /*
	    TODO: shorten 2 ledger lines for the case natural +
	    downstem.
	   */
	}

      out.add_stencil (Note_head::brew_ledger_lines (me, pos, interspaces,
						      ledger_size,
						      left_shorten,
						      false));
    }
  return out;
}


MAKE_SCHEME_CALLBACK (Note_head,print,1);
SCM
Note_head::print (SCM smob)  
{
  Grob *me = unsmob_grob (smob);

  /*
    ledgers don't take space. See top of file.
   */
  return internal_print (me, true).smobbed_copy ();
}

/*
  Compute the width the head without ledgers.

  -- there used to be some code from the time that ledgers
  did take space. Nowadays, we can simply take the standard extent.
 */
Interval
Note_head::head_extent (Grob *me, Axis a)
{
  SCM brewer = me->get_property ("print-function");
  if (brewer == Note_head::print_proc)
    {
      Stencil mol = internal_print (me, false);
  
      if (!mol.is_empty ())
	return mol.extent (a);
    }
  else
    {
      Stencil * mol = me->get_stencil ();
      if (mol)
	return  mol->extent (a) ;
    }
  
  return Interval (0,0);
}

/*
  This is necessary to prevent a cyclic dependency: the appearance of
  the ledgers depends on positioning, so the Grob::get_stencil () can
  not be used for determining the note head extent.
  
 */ 
MAKE_SCHEME_CALLBACK (Note_head,extent,2);
SCM
Note_head::extent (SCM smob, SCM axis)  
{
  Grob *me = unsmob_grob (smob);

  return ly_interval2scm (head_extent (me, (Axis) ly_scm2int (axis)));
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
  if (ly_vector_p (names))
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

  int pos = Staff_symbol_referencer::get_rounded_position (me);
  int interspaces = Staff_symbol_referencer::line_count (me)-1;
  if (abs (pos) - interspaces > 1)
    {
      Interval hd = m.extent (X_AXIS);
      hd.widen ( hd.length ()/4);
      m.add_stencil (brew_ledger_lines (me, pos, interspaces, hd, 0, false));
    }

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
      if (!ly_symbol_p (style))
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
  if (!ly_procedure_p (v))
    return 0.0;
  
  SCM result = scm_call_2 (v, me->self_scm (), scm_int2num (a));
  if (!ly_pair_p (result))
    return 0.0;

  result = (a == X_AXIS) ? ly_car (result) : ly_cdr (result);
  
  return robust_scm2double (result,0);
}

int
Note_head::get_balltype (Grob*me) 
{
  SCM s = me->get_property ("duration-log");
  return ly_number_p (s) ? ly_scm2int (s) <? 2 : 0;
}

ADD_INTERFACE (Note_head,"note-head-interface",
  "Note head",
  "note-names glyph-name-procedure accidental-grob style stem-attachment-function");

