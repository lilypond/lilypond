/*
  mensural-ligature-engraver.cc -- implement Mensural_ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002--2004 Juergen Reuter <reuter@ipd.uka.de>
 */

#include "mensural-ligature.hh"
#include "coherent-ligature-engraver.hh"
#include "event.hh"
#include "warn.hh"
#include "item.hh"
#include "spanner.hh"
#include "rod.hh"
#include "paper-column.hh"
#include "note-column.hh"
#include "rhythmic-head.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "output-def.hh"
#include "font-interface.hh"

/*
 * TODO: My resources on Franco of Cologne's rules claim that his
 * rules map ligature<->mensural timing in a non-ambigous way, but in
 * fact, as presented in these resources, the rules become ambigous as
 * soon as there appear durations other than breves within a ligature
 * with more than two heads (ligatura ternaria etc.).  Hence, the
 * below implementation is an approximation of what I think the rules
 * could look like if forced to be non-ambigous.  This should be
 * further investigated.
 *
 * TODO: The automat is quite complicated, and its design is error
 * prone (and most probably, it behaves wrong for some very special
 * cases).  Maybe we can find a better paradigm for modelling Franco
 * of Cologne's rules?
 *
 * TODO: dotted heads: when applying Franco of Cologne's mapping, put
 * dots *above* (rather than after) affected ligature heads.
 *
 * TODO: prohibit multiple voices within a ligature.
 *
 * TODO: enhance robustness: in case of an illegal ligature (e.g. the
 * user events for a ligature that contains a minima or STATE_ERROR
 * is reached), automatically break the ligature into smaller, valid
 * pieces.
 */
class Mensural_ligature_engraver : public Coherent_ligature_engraver
{

protected:
  virtual Spanner *create_ligature_spanner ();
  virtual void build_ligature (Spanner *ligature, Array<Grob_info> primitives);

public:
  TRANSLATOR_DECLARATIONS (Mensural_ligature_engraver);

private:
  int apply_transition (Array<Grob_info> primitives,
			int state, int input, int i);
  void transform_heads (Array<Grob_info> primitives);
  void propagate_properties (Spanner *ligature, Array<Grob_info> primitives);
  void fold_up_primitives (Array<Grob_info> primitives);
  void join_primitives (Array<Grob_info> primitives);
};


Mensural_ligature_engraver::Mensural_ligature_engraver ()
{
}

Spanner *
Mensural_ligature_engraver::create_ligature_spanner ()
{
  return make_spanner ("MensuralLigature", SCM_EOL);
}

/*
 * The following lines implement a finite state automat.  Given a
 * sequence of durations (Longa, Brevis, Semibrevis) or
 * end-of-ligature-event as input, the automat outputs a sequence of
 * events for grobs that form a proper ligature.
 */

/*
 * This enumeration represents the set of possible input values to the
 * automat.  There may (potentially) be any sequence of Longa, Brevis,
 * and Semibrevis duration symbols fed into the automat, with a final
 * EndOfLigature symbol to terminate the ligature.  Other durations
 * are explicitly prohibited.  Depending on the note's pitch of the
 * preceding and the current input, the melodic line may be ascending
 * or descending.  Per definition, the melodic line must either ascend
 * or descend, because if the pitches were twice the same, the two
 * notes would be merged into a single one (as long as not resulting
 * in a prohibited duration).  In the case of the EndOfLigature
 * symbol, the melodic line is undefined (but we still have ascending
 * and descending case for the sake of consistency, making the automat
 * simpler).
 */
enum Ligature_input
{
  // Ascending/Descending Longa/Brevis/Semibrevis/EndOfLigature
  INPUT_AL = 0,
  INPUT_DL,
  INPUT_AB,
  INPUT_DB,
  INPUT_AS,
  INPUT_DS,
  INPUT_AE,
  INPUT_DE,
};

/*
 * This enumeration represents all possible internal states of the
 * automat.  Besides the generic states START, ERROR, and END, the
 * remaining states L, B, S, and SS describe pending values from the
 * sequence of input values that have not yet been transformed to
 * proper output values, including the melodic direction
 * (ascending/descending) for state L.
 */
enum Ligature_state
{
  // aL = ascending Longa, dL descending Longa, B = Brevis, S =
  // Semibrevis, SS = 2 Semibreves
  STATE_START = 0,
  STATE_aL,
  STATE_dL,
  STATE_B,
  STATE_S,
  STATE_SS,
  STATE_ERROR,
  STATE_END,
};

/*
 * The following array represents the transitions of the automat:
 * given some state and input, it maps to a new state, according (with
 * the limitations as described above) to the rules of Franco of
 * Cologne.
 */
const int/*new state*/ transition_state[/*old state*/][8/*input*/] =
{
  {STATE_aL,    STATE_dL,    STATE_B,     STATE_B,
   STATE_S,     STATE_S,     STATE_ERROR, STATE_ERROR}, // was: STATE_START
  {STATE_aL,    STATE_dL,    STATE_B,     STATE_START,
   STATE_ERROR, STATE_ERROR, STATE_END,   STATE_END},   // was: STATE_aL
  {STATE_aL,    STATE_dL,    STATE_B,     STATE_START,
   STATE_ERROR, STATE_ERROR, STATE_END,   STATE_END},   // was: STATE_dL
  {STATE_aL,    STATE_dL,    STATE_B,     STATE_START,
   STATE_ERROR, STATE_ERROR, STATE_END,   STATE_END},   // was: STATE_B
  {STATE_ERROR, STATE_ERROR, STATE_ERROR, STATE_ERROR,
   STATE_SS,    STATE_SS,    STATE_ERROR, STATE_ERROR}, // was: STATE_S
  {STATE_aL,    STATE_dL,    STATE_B,     STATE_B,
   STATE_S,     STATE_S,     STATE_END,   STATE_END},   // was: STATE_SS
  {STATE_ERROR, STATE_ERROR, STATE_ERROR, STATE_ERROR,
   STATE_ERROR, STATE_ERROR, STATE_ERROR, STATE_ERROR}, // was: STATE_ERROR
  {STATE_ERROR, STATE_ERROR, STATE_ERROR, STATE_ERROR,
   STATE_ERROR, STATE_ERROR, STATE_ERROR, STATE_ERROR}, // was: STATE_END
};

/*
 * The following array represents the output of the automat while
 * switching from one state to another: given some state and input, it
 * maps to the output produced when switching to the next state,
 * according (with the limitations as described above) to the rules of
 * Franco of Cologne.
 */
const int/*output*/ transition_output[/*old state*/][8/*input*/] =
{
  {MLP_NONE,  MLP_NONE,  MLP_NONE,  MLP_NONE,
   MLP_NONE,  MLP_NONE,  MLP_NONE,  MLP_NONE}, // was: STATE_START
  {MLP_sc,    MLP_ss,    MLP_sc,    MLP_LB,
   MLP_NONE,  MLP_NONE,  MLP_sc,    MLP_sc},   // was: STATE_aL
  {MLP_sc,    MLP_ss,    MLP_sc,    MLP_LB,
   MLP_NONE,  MLP_NONE,  MLP_ss,    MLP_ss},   // was: STATE_dL
  {MLP_ss,    MLP_cs,    MLP_ss,    MLP_BB,
   MLP_NONE,  MLP_NONE,  MLP_ss,    MLP_ss} ,  // was: STATE_B
  {MLP_NONE,  MLP_NONE,  MLP_NONE,  MLP_NONE,
   MLP_NONE,  MLP_NONE,  MLP_NONE,  MLP_NONE}, // was: STATE_S
  {MLP_SS,    MLP_SS,    MLP_SS,    MLP_SS,
   MLP_SS,    MLP_SS,    MLP_SS,    MLP_SS} ,  // was: STATE_SS
  {MLP_NONE,  MLP_NONE,  MLP_NONE,  MLP_NONE,
   MLP_NONE,  MLP_NONE,  MLP_NONE,  MLP_NONE}, // was: STATE_ERROR
  {MLP_NONE,  MLP_NONE,  MLP_NONE,  MLP_NONE,
   MLP_NONE,  MLP_NONE,  MLP_NONE,  MLP_NONE}, // was: STATE_END
};

int
Mensural_ligature_engraver::apply_transition (Array<Grob_info> primitives,
					      int state, int input, int i)
{
  int output = transition_output[state][input];
  Item *last_last_primitive = (i > 1) ?
    dynamic_cast<Item*> (primitives[i-2].grob_) : 0;
  Item *last_primitive = (i > 0) ?
    dynamic_cast<Item*> (primitives[i-1].grob_) : 0;
  Item *primitive = (i < primitives.size ()) ?
    dynamic_cast<Item*> (primitives[i].grob_) : 0;
  switch (output)
    {
      case MLP_NONE:
	// skip note head, expecting a primitive with two note heads
	break;
      case MLP_sc:
      case MLP_ss:
      case MLP_cs:
	// primitive with single note head
	if (!last_primitive)
	  {
	    programming_error ("last_primitive undefined");
	    break;
	  }
	last_primitive->set_property ("primitive", scm_int2num (output));
	break;
      case MLP_BB:
      case MLP_LB:
	// primitive with two note heads
	if (!last_primitive)
	  {
	    programming_error ("last_primitive undefined");
	    break;
	  }
	if (!primitive)
	  {
	    programming_error ("primitive undefined");
	    break;
	  }
	last_primitive->set_property ("primitive", scm_int2num (output));
	primitive->set_property ("primitive", scm_int2num (MLP_NONE));
	break;
      case MLP_SS:
	// delayed primitive with two note heads
	if (!last_last_primitive)
	  {
	    programming_error ("last_last_primitive undefined");
	    break;
	  }
	if (!last_primitive)
	  {
	    programming_error ("last_primitive undefined");
	    break;
	  }
	last_last_primitive->set_property ("primitive", scm_int2num (output));
	last_primitive->set_property ("primitive", scm_int2num (MLP_NONE));
	break;
      default:
	programming_error (_f ("unexpected case fall-through"));
	break;
    }
  return transition_state[state][input];
}

void
Mensural_ligature_engraver::transform_heads (Array<Grob_info> primitives)
{
  if (primitives.size () < 2)
    {
      warning (_f ("ligature with less than 2 heads -> skipping"));
      return;
    }
  int state = STATE_START;
  Pitch last_pitch, pitch;
  bool have_last_pitch = 0, have_pitch = 0;
  for (int i = 0; i < primitives.size (); i++) {
    last_pitch = pitch;
    have_last_pitch = have_pitch;
    Grob_info info = primitives[i];
    int duration_log =
      Note_head::get_balltype (dynamic_cast<Item*> (info.grob_));

    Music *nr = info.music_cause ();
    
    /*
    ugh. why not simply check for pitch? 
     */
    if (!nr->is_mus_type ("note-event"))
      {
	info.music_cause ()->origin ()->warning (_f ("can not determine pitch of ligature primitive -> skipping"));
	i++;
	state = STATE_START;
	have_pitch = 0;
	continue;
      }
    else
      {
	pitch = *unsmob_pitch (nr->get_property ("pitch"));
	have_pitch = 1;
      }

    int delta_pitch;

    if (!have_last_pitch)
      {
	delta_pitch = 0; // first pitch; delta undefined
      }
    else
      {
	delta_pitch = (pitch.steps () - last_pitch.steps ());
	if (Pitch::compare (last_pitch, pitch) == 0)
	  {
	    info.music_cause ()->origin ()->warning (_f ("prime interval within ligature -> skipping"));
	    i++;
	    state = STATE_START;
	    have_pitch = 0;
	    continue;
	  }
      }

    if ((duration_log < -2) || (duration_log > 0))
      {
	info.music_cause ()->origin ()->warning (_f ("mensural ligature: duration none of L, B, S -> skipping"));
	i++;
	state = STATE_START;
	have_pitch = 0;
	continue;
      }

    int input = (duration_log + 2) * 2 + ((delta_pitch < 0) ? 1 : 0);
    state = apply_transition (primitives, state, input, i);
    // TODO: if (state == STATE_ERROR) { ... }
  }

  state = apply_transition (primitives, state, INPUT_AE, primitives.size ());
  // TODO: if (state == STATE_ERROR) { ... }
}

/*
 * A MensuralLigature grob consists of a bunch of NoteHead grobs that
 * are glued together.  It (a) does not make sense to change
 * properties like thickness or flexa-width from one head to the next
 * within a ligature (this would totally screw up alignment), and (b)
 * some of these properties (like flexa-width) are specific to
 * e.g. the MensuralLigature (as in contrast to e.g. LigatureBracket),
 * and therefore should not be handled in the NoteHead code (which is
 * also used by LigatureBracket).  Therefore, we let the user control
 * these properties via the concrete Ligature grob (like
 * MensuralLigature) and then copy these properties as necessary to
 * each of the NoteHead grobs.  This is what
 * propagate_properties () does.
 */
void
Mensural_ligature_engraver::propagate_properties (Spanner *ligature,
						  Array<Grob_info> primitives)
{
  Real thickness = robust_scm2double (ligature->get_property ("thickness"), 1.4);
  thickness *= ligature->get_layout ()->get_dimension (ly_symbol2scm ("linethickness"));

  Real head_width =
    Font_interface::get_default_font (ligature)->
    find_by_name ("noteheads--1mensural").extent (X_AXIS).length ();
    Real flexa_width = robust_scm2double (ligature->get_property ("flexa-width"), 2);
  flexa_width *= Staff_symbol_referencer::staff_space (ligature);

  Real half_flexa_width = 0.5 * (flexa_width + thickness);

  for (int i = 0; i < primitives.size (); i++)
    {
      Item *primitive = dynamic_cast<Item*> (primitives[i].grob_);
      int output = scm_to_int (primitive->get_property ("primitive"));
      primitive->set_property ("thickness",
				    scm_make_real (thickness));
      switch (output) {
	case MLP_NONE:
	  primitive->set_property ("head-width",
					scm_make_real (half_flexa_width));
	  break;
	case MLP_sc:
	case MLP_ss:
	case MLP_cs:
	  primitive->set_property ("head-width",
					scm_make_real (head_width));
	  break;
	case MLP_BB:
	case MLP_LB:
	case MLP_SS:
	  primitive->set_property ("head-width",
					scm_make_real (half_flexa_width));
	  primitive->set_property ("flexa-width",
					scm_make_real (flexa_width));
	  break;
	default:
	  programming_error (_f ("unexpected case fall-through"));
	  break;
      }
    }
}

void
Mensural_ligature_engraver::fold_up_primitives (Array<Grob_info> primitives)
{
  Item *first = 0;
  Real distance = 0;
  for (int i = 0; i < primitives.size (); i++)
    {
      Item *current = dynamic_cast<Item*> (primitives[i].grob_);
      if (i == 0)
	{
	  first = current;
	}

      get_set_column (current, first->get_column ());

      if (i > 0)
	{
	  current->translate_axis (distance, X_AXIS);
	}

      distance +=
	scm_to_double (current->get_property ("head-width")) -
	scm_to_double (current->get_property ("thickness"));
    }
}

void
Mensural_ligature_engraver::join_primitives (Array<Grob_info> primitives)
{
  Pitch last_pitch;
  for (int i = 0; i < primitives.size (); i++)
    {
      Grob_info info = primitives[i];
      Pitch pitch = *unsmob_pitch (info.music_cause ()->get_property ("pitch"));
      if (i > 0)
        {
	  Item *primitive = dynamic_cast<Item*> (info.grob_);
	  int output = scm_to_int (primitive->get_property ("primitive"));
	  if (output & MLP_ANY)
	    {
	      int delta_pitch = (pitch.steps () - last_pitch.steps ());
	      primitive->set_property ("join-left-amount",
					    scm_int2num (delta_pitch));
	    }
	}
      last_pitch = pitch;
    }
}

void
Mensural_ligature_engraver::build_ligature (Spanner *ligature,
					    Array<Grob_info> primitives)
{
  transform_heads (primitives);
  propagate_properties (ligature, primitives);
  fold_up_primitives (primitives);
  join_primitives (primitives);
}

ENTER_DESCRIPTION (Mensural_ligature_engraver,
/* descr */       "Handles Mensural_ligature_events by glueing special ligature heads together.",
/* creats*/       "MensuralLigature",
/* accepts */     "ligature-event",
/* acks  */      "note-head-interface rest-interface",
/* reads */       "",
/* write */       "");
