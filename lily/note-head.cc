/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>
#include <ctype.h>

#include "misc.hh"
#include "dots.hh"
#include "note-head.hh"
#include "warn.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "musical-request.hh"
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

Molecule
Note_head::brew_ledger_lines (Grob *me,
                              int pos,
                              int interspaces,
                              Interval x_extent,
                              bool take_space)
{
  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;
  int lines_i = abs (pos) < interspaces
    ? 0
    : (abs (pos) - interspaces) / 2;
  Molecule molecule = Molecule();

  if (lines_i)
    {
      Real ledgerlinethickness =
	(me->get_paper ()->get_var ("ledgerlinethickness"));
      Real blotdiameter = ledgerlinethickness;
      //	(me->get_paper ()->get_var ("blotdiameter"));
      Interval y_extent =
	Interval (-0.5*(ledgerlinethickness),
		  +0.5*(ledgerlinethickness));
      Box ledger_line (x_extent, y_extent);

      Molecule proto_ledger_line =
	Lookup::roundfilledbox (ledger_line, blotdiameter);
      
      if (!take_space)
        proto_ledger_line.set_empty (true);

      Direction dir = (Direction)sign (pos);
      Real offs = (Staff_symbol_referencer::on_staffline (me, pos))
        ? 0.0
        : -dir * inter_f;
      for (int i = 0; i < lines_i; i++)
        {
          Molecule ledger_line (proto_ledger_line);
          ledger_line.translate_axis (-dir * inter_f * i * 2 + offs, Y_AXIS);
          molecule.add_molecule (ledger_line);
        }
    }

  return molecule;
}

Molecule
internal_brew_molecule (Grob *me,  bool ledger_take_space)
{
  SCM style  = me->get_grob_property ("style");
  if (!gh_symbol_p (style))
    {
      return Molecule();
    }

  /*
    ugh: use gh_call () / scm_apply ().

    UGH: use grob-property.
  */
  SCM log = gh_int2scm (Note_head::get_balltype (me));
  SCM exp = scm_list_n (ly_symbol2scm ("find-notehead-symbol"), log,
			ly_quote_scm (style),
			SCM_UNDEFINED);
  SCM scm_pair = scm_primitive_eval (exp);
  SCM scm_font_char = ly_car (scm_pair);
  SCM scm_font_family = ly_cdr (scm_pair);
  String font_char = "noteheads-" + ly_scm2string (scm_font_char);
  String font_family = ly_scm2string (scm_font_family);

  me->set_grob_property("font-family", ly_symbol2scm (font_family.to_str0 ()));
  Molecule out =
    Font_interface::get_default_font (me)->find_by_name (font_char);
  if (out.empty_b())
    {
      me->warning (_f ("Symbol `%s' not found in family `%s'",
		       font_char.to_str0 (), font_family.to_str0 ()));
    }
  int interspaces = Staff_symbol_referencer::line_count (me)-1;
  int pos = (int)rint (Staff_symbol_referencer::get_position (me));
  if (abs (pos) - interspaces > 1)
    {
      Interval hd = out.extent (X_AXIS);
      Real left_ledger_protusion = hd.length ()/4;
      Real right_ledger_protusion = left_ledger_protusion;

      if (unsmob_grob(me->get_grob_property ("accidental-grob")))
	{
	  /*
	    make a little room for accidentals.
	  
	    TODO: this will look silly if a chord has ledger lines,
	    and only the bottom note has an accidental.
	  */
      	  
	  left_ledger_protusion *= 0.66;
	  right_ledger_protusion *= 0.9; 
	}

      Interval l_extents = Interval (hd[LEFT] - left_ledger_protusion,
				     hd[RIGHT] + right_ledger_protusion);
      out.add_molecule (Note_head::brew_ledger_lines (me, pos, interspaces,
						      l_extents,
						      ledger_take_space));
    }
  return out;
}


MAKE_SCHEME_CALLBACK (Note_head,brew_molecule,1);
SCM
Note_head::brew_molecule (SCM smob)  
{
  Grob *me = unsmob_grob (smob);

  /*
    ledgers don't take space. See top of file.
   */
  return internal_brew_molecule (me, false).smobbed_copy ();
}

/*
  Compute the width the head without ledgers.

  -- there used to be some code from the time that ledgers
  did take space. Nowadays, we can simply take the standard extent.
 */
Interval
Note_head::head_extent (Grob *me, Axis a)
{
  Molecule * mol = me->get_molecule();
  return mol ? mol ->extent (a) : Interval(0,0);
}



MAKE_SCHEME_CALLBACK (Note_head,brew_ez_molecule,1);

SCM
Note_head::brew_ez_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  int l = Note_head::get_balltype (me);

  int b = (l >= 2);

  SCM cause = me->get_grob_property ("cause");
  SCM spitch = unsmob_music (cause)->get_mus_property ("pitch");
  Pitch* pit =  unsmob_pitch (spitch);

  char s[2] = "a";
  s[0] = (pit->notename_ + 2)%7 + 'a';
  s[0] = toupper (s[0]);
  
  SCM charstr = scm_makfrom0str (s);
  
  SCM at = scm_list_n (ly_symbol2scm ("ez-ball"),
		       charstr,
		       gh_int2scm (b),
		       gh_int2scm (1-b),
		       SCM_UNDEFINED);
  Box bx (Interval (0, 1.0), Interval (-0.5, 0.5));
  Molecule m (bx, at);

  int pos = (int)rint (Staff_symbol_referencer::get_position (me));
  int interspaces = Staff_symbol_referencer::line_count (me)-1;
  if (abs (pos) - interspaces > 1)
    {
      Interval hd = m.extent (X_AXIS);
      Real hw = hd.length ()/4;
      Interval extent = Interval (hd[LEFT] - hw, hd[RIGHT] + hw);
      m.add_molecule (brew_ledger_lines (me, pos, interspaces, extent, false));
    }

  return m.smobbed_copy ();
}


Real
Note_head::stem_attachment_coordinate (Grob *me, Axis a)
{
  SCM v = me->get_grob_property ("stem-attachment-function");

  if (!gh_procedure_p (v))
    return 0.0;

  SCM st = me->get_grob_property ("style");
  SCM log = gh_int2scm (get_balltype (me));
  SCM result = gh_apply (v, scm_list_n (st, log, SCM_UNDEFINED));

  if (!gh_pair_p (result))
    return 0.0;

  result = (a == X_AXIS) ? ly_car (result) : ly_cdr (result);
  
  return gh_number_p (result) ?  gh_scm2double (result) : 0.0;
}

int
Note_head::get_balltype (Grob*me) 
{
  SCM s = me->get_grob_property ("duration-log");
  return gh_number_p (s) ? gh_scm2int (s) <? 2 : 0;
}

ADD_INTERFACE (Note_head,"note-head-interface",
  "Note head",
  "accidental-grob style stem-attachment-function");

