/*
  ambitus.cc -- implement Ambitus

  source file of the GNU LilyPond music typesetter

  (c) 2002--2004 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "staff-symbol-referencer.hh"
#include "pitch.hh"
#include "ambitus.hh"
#include "stencil.hh"
#include "note-head.hh"
#include "item.hh"
#include "font-interface.hh"
#include "output-def.hh"
#include "lookup.hh"

/*
  UGH UGH UGH

  This does 3 things at one:

  - acc positioning
  - drawing accidentals
  - drawing note heads

  It confuses interpretation & formatting.

  UGH.
  --hwn.
 */

/*
 * TODO: note-head collision handling
 *
 * TODO: accidentals collision handling
 *
 * TODO: alternative representation: adding the ambitus as text script
 * to the instrument name (e.g. "Soprano (c^1 - f^2)").
 *
 * FIXME: Accidentals are too close at the note heads (it seems that
 * the extent of the ledger lines is ignored).
 *
 * TODO: If (depending on breakAlignOrder) ambitus is put behind
 * key-signature, then do not repeat accidentals that already appear
 * in the key signature.
 *
 * FIXME: A staff containing more than a single context will result in
 * multiple ambitus grobs per staff.  This is basically ok, but there is
 * currently no proper collision handling for this case.
 *
 * TODO: make ignore_octave and force_accidental of function
 * number_accidentals accessible via grob properties.
 */

/**
  Given a pitch and a key_signature, decide what accidentals to show.
 
  Possible return values:
 
  0: do not show any accidental
  1: show pitch->alteration_ only
  2: show pitch->get_alteration, preceded by a natural sign

  UGH: code duplication! See accidental-engraver.
 
 */
static int
number_accidentals (SCM key_signature, Pitch *pitch,
		    bool ignore_octave_b, bool force_accidental)
{
  int notename = pitch->get_notename ();
  int octave = pitch->get_octave ();
  int alteration = pitch->get_alteration ();

  if (force_accidental) // ignore key signature
    return 1;

  
#if DEBUG_AMBITUS
  scm_display (key_signature, scm_current_output_port ());
#endif

  SCM prev;
  if (ignore_octave_b)
    prev = ly_assoc_cdr (scm_int2num (notename), key_signature);
  else
    prev = scm_assoc (scm_cons (scm_int2num (octave), scm_int2num (notename)),
		     key_signature);

  /* should really be true unless prev == SCM_BOOL_F */
  if (ly_c_pair_p (prev) && ly_c_pair_p (ly_cdr (prev)))
    {
      prev = scm_cons (ly_car (prev), ly_cadr (prev));
    }

  /* If an accidental was not found */
  if (prev == SCM_BOOL_F)
    prev = scm_assoc (scm_int2num (notename), key_signature);

  SCM prev_acc = (prev == SCM_BOOL_F) ? scm_int2num (0) : ly_cdr (prev);
  int sig_alteration = ly_c_number_p (prev_acc) ? ly_scm2int (prev_acc) : 0;

  if (alteration == sig_alteration) // no accidental at all needed
    return 0;

  if ((alteration == 0) && (sig_alteration != 0)) // need ordinary natural
    return 2;

  if (sig_alteration == 0) // use pitch's alteration
    return 1;

  return 2;
}



void
add_accidentals (Item *me, Stencil *head, int num_acc,
		 Pitch *pitch, String accidentals_style, Real yoffs)
{
  if (!num_acc)
    return;
  if (pitch->get_alteration ())
    {
      Stencil accidental (Font_interface::get_default_font (me)->
			   find_by_name (String ("accidentals-") +
					 accidentals_style +
					 to_string (pitch->get_alteration ())));
      accidental.translate_axis (yoffs, Y_AXIS);
      head->add_at_edge (X_AXIS,  LEFT, accidental, 0.1, 0);
    }
  if (num_acc == 2)
    {
      Stencil natural (Font_interface::get_default_font (me)->
			find_by_name (String ("accidentals-") +
				      accidentals_style +
				      to_string ("0")));
      natural.translate_axis (yoffs, Y_AXIS);
      head->add_at_edge (X_AXIS,  LEFT, natural, 0.1, 0);
    }
}

MAKE_SCHEME_CALLBACK (Ambitus,print,1);
SCM
Ambitus::print (SCM smob)
{
  Item *me = (Item*) unsmob_grob (smob);
  Stencil stencil;

  SCM scm_note_head_style = me->get_property ("note-head-style");
  String note_head_style;
  if (ly_c_symbol_p (scm_note_head_style))
    {
      String note_head_style =
	ly_symbol2string (scm_note_head_style);
    }
  else
    {
      note_head_style = String ("noteheads-2");
    }
  if (Font_interface::get_default_font (me)->find_by_name (note_head_style).is_empty ())
    {
      String message = "Ambitus: no such note head: `" + note_head_style + "'";
      me->warning (_ (message.to_str0 ()));
      return SCM_EOL;
    }

  /*
    FIXME: Use positions. 
   */
  int p_min, p_max;
  Slice posns = get_positions(me);
  
  p_min = posns[LEFT];
  p_max = posns[RIGHT];

  // create heads
  Stencil head_min =
    Font_interface::get_default_font (me)->find_by_name (note_head_style);
  head_min.translate_axis (0.5*p_min, Y_AXIS);
  Stencil head_max =
    Font_interface::get_default_font (me)->find_by_name (note_head_style);
  head_max.translate_axis (0.5*p_max, Y_AXIS);

  // join heads
  if (to_boolean (me->get_property ("join-heads")) &&
      ((p_max - p_min) >= 3))
    {
      Real linethickness = me->get_paper ()->get_dimension (ly_symbol2scm ("linethickness"));
      Real blotdiameter = me->get_paper ()->get_dimension (ly_symbol2scm ("blotdiameter"));
      Interval x_extent = 0.5 * Interval (-linethickness, +linethickness);
      Interval y_extent = 0.5 * Interval (p_min + 1.35, p_max - 1.35);
      Box line_box (x_extent, y_extent);
      Stencil line = Lookup::round_filled_box (line_box, blotdiameter);
      line.translate_axis (0.5 * head_min.extent (X_AXIS).length (), X_AXIS);
      stencil.add_stencil (line);
    }

  
  // add accidentals
  SCM key_signature = me->get_property ("key-signature");
  SCM scm_accidentals_style = me->get_property ("accidentals-style");
  String accidentals_style;
  if (ly_c_symbol_p (scm_accidentals_style))
    {
      accidentals_style =
	ly_symbol2string (scm_accidentals_style);
    }
  else
    {
      accidentals_style = String ("");
    }
  
  int num_acc;
  Pitch *pitch_min = unsmob_pitch (me->get_property ("pitch-min"));
  Pitch *pitch_max = unsmob_pitch (me->get_property ("pitch-max"));
  num_acc = number_accidentals (key_signature, pitch_min, true, false);
  add_accidentals (me, &head_min, num_acc, pitch_min,
		   accidentals_style, 0.5 * p_min);
  num_acc = number_accidentals (key_signature, pitch_max, true, false);
  add_accidentals (me, &head_max, num_acc, pitch_max,
		   accidentals_style, 0.5 * p_max);

  // add heads
  stencil.add_stencil (head_min);
  stencil.add_stencil (head_max);

  return stencil.smobbed_copy ();
}

ADD_INTERFACE (Ambitus, "ambitus-interface",
  "An object that represents the pitch range of a voice.",
  "c0-position pitch-min pitch-max accidentals note-head-style accidentals-style join-heads");
