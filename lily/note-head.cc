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
#include "debug.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "musical-request.hh"
#include "rhythmic-head.hh"

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

#include "staff-symbol-referencer.hh"

/*
  build a ledger line for small pieces.
 */
Molecule
Note_head::ledger_line (Grob *me, Interval xwid) 
{
  Drul_array<Molecule> endings;
  endings[LEFT] = Font_interface::get_default_font (me)->find_by_name ("noteheads-ledgerending");
  Molecule *e = &endings[LEFT];
  endings[RIGHT] = *e;
  
  Real thick = e->extent (Y_AXIS).length ();
  Real len = e->extent (X_AXIS).length () - thick;

  Molecule total;
  Direction d = LEFT;
  do {
    endings[d].translate_axis (xwid[d] - endings[d].extent (X_AXIS)[d], X_AXIS);
    total.add_molecule (endings[d]);    
  } while ((flip (&d)) != LEFT);

  Real xpos = xwid [LEFT] + len;

  while (xpos + len + thick /2 <= xwid[RIGHT])
    {
      e->translate_axis (len, X_AXIS);
      total.add_molecule (*e);
      xpos += len;
    }

  return total;
}


Molecule
Note_head::ledger_lines (Grob*me,
			 bool take_space,
			 int count, Direction dir, Interval idw)
{
  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;

  /*
    idw ?

    (who's that ?  :-)


    --hwn 
   */
  Molecule ledger (ledger_line (me, idw));

  if (!take_space)
    ledger.set_empty (true);
  
  Real offs = (Staff_symbol_referencer::on_staffline (me))
    ? 0.0
    : -dir * inter_f;

  Molecule legs;
  for (int i=0; i < count; i++)
    {
      Molecule s (ledger);
      s.translate_axis (-dir * inter_f * i*2 + offs,
			Y_AXIS);
      legs.add_molecule (s);
    }

  return legs;
}

Molecule
internal_brew_molecule (Grob *me,  bool ledger_take_space)
{
  int sz = Staff_symbol_referencer::line_count (me)-1;
  int p = (int)  rint (Staff_symbol_referencer::position_f (me));
  int streepjes_i = abs (p) < sz 
    ? 0
    : (abs (p) - sz) /2;

  SCM style  = me->get_grob_property ("style");
  if (!gh_symbol_p (style))
    {
      return Molecule();
    }

  /*
    ugh: use gh_call () / scm_apply ().

    UGH: use grob-property.
  */
  SCM log = gh_int2scm (Rhythmic_head::balltype_i (me));
  SCM exp = scm_list_n (ly_symbol2scm ("find-notehead-symbol"), log,
			ly_quote_scm (style),
			SCM_UNDEFINED);
  String name = "noteheads-" + ly_scm2string (scm_primitive_eval (exp));
  Molecule out = Font_interface::get_default_font (me)->find_by_name (name);

  if (streepjes_i) 
    {
      Direction dir = (Direction)sign (p);
      Interval hd = out.extent (X_AXIS);
      Real left_ledger_protusion = hd.length ()/4;
      Real right_ledger_protusion = left_ledger_protusion;

      if (unsmob_grob(me->get_grob_property ("accidentals-grob")))
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
      out.add_molecule (Note_head::ledger_lines (me, ledger_take_space,
						 streepjes_i, dir, l_extents));
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
 */
Interval
Note_head::head_extent (Grob *me, Axis a)
{
  return internal_brew_molecule (me, false).extent (a);
}

bool
Note_head::has_interface (Grob*m)
{
  return m&& m->has_interface (ly_symbol2scm ("note-head-interface"));
}


MAKE_SCHEME_CALLBACK (Note_head,brew_ez_molecule,1);

SCM
Note_head::brew_ez_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  int l = Rhythmic_head::balltype_i (me);

  int b = (l >= 2);

  SCM cause = me->get_grob_property ("cause");
  SCM spitch = unsmob_music (cause)->get_mus_property ("pitch");
  Pitch* pit =  unsmob_pitch (spitch);

  char s[2] = "a";
  s[0] = (pit->notename_i_ + 2)%7 + 'a';
  s[0] = toupper (s[0]);
  
  SCM charstr = ly_str02scm (s);
  
  SCM at = scm_list_n (ly_symbol2scm ("ez-ball"),
		       charstr,
		       gh_int2scm (b),
		       gh_int2scm (1-b),
		       SCM_UNDEFINED);
  Box bx (Interval (0, 1.0), Interval (-0.5, 0.5));
  Molecule m (bx, at);
  int p = (int)  rint (Staff_symbol_referencer::position_f (me));

  int sz = Staff_symbol_referencer::line_count (me)-1;
  int streepjes_i = abs (p) < sz 
    ? 0
    : (abs (p) - sz) /2;

 if (streepjes_i)
   {
      Direction dir = (Direction)sign (p);
      Interval hd = m.extent (X_AXIS);
      Real hw = hd.length ()/4;
      m.add_molecule (ledger_lines (me, false, streepjes_i, dir,
				    Interval (hd[LEFT] - hw,
						hd[RIGHT] + hw)));
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
  SCM result = gh_apply (v, scm_list_n (st, SCM_UNDEFINED));

  if (!gh_pair_p (result))
    return 0.0;

  result = (a == X_AXIS) ? ly_car (result) : ly_cdr (result);
  
  return gh_number_p (result) ?  gh_scm2double (result) : 0.0;
}

ADD_INTERFACE (Note_head,"note-head-interface",
  "Note head",
  "accidentals-grob style stem-attachment-function");

