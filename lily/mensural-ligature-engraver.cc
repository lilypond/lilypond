/*
  mensural-ligature-engraver.cc -- implement Mensural_ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (C) 2002 Juergen Reuter <reuter@ipd.uka.de>
 */

#include "mensural-ligature.hh"
#include "ligature-engraver.hh"
#include "musical-request.hh"
#include "warn.hh"
#include "item.hh"
#include "spanner.hh"
#include "rod.hh"
#include "paper-column.hh"
#include "note-column.hh"
#include "rhythmic-head.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "paper-def.hh"
#include "font-interface.hh"

/*
 * TODO: local accidentals: collect accidentals that occur within a
 * ligature and put them before the ligature.  If an accidental
 * changes within a ligature, print a warning (user error) and ignore
 * any further accidental for that pitch within that ligature
 * (actually, in such a case, the user should split the ligature into
 * two separate ligatures).  Similarly, any object that, in ordinary
 * notation, may be put to the left or to the right of a
 * note-head/ligature-head, should be collected and put before or
 * after the ligature.
 *
 * TODO: make spacing more robust: do not screw up spacing if user
 * erroneously puts rest in ligature.
 *
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
 * TODO: for each ligature, add Rod that represents the total length
 * of the ligature (to preemptively avoid collision with adjacent
 * notes); or maybe just additionally create a mensural-ligature grob
 * (via Mensural_ligature::brew_molecule(SCM)) that just consists of a
 * bounding box around all primitives of the ligature.
 *
 * TODO: enhance robustness: in case of an illegal ligature (e.g. the
 * user requests for a ligature that contains a minima or STATE_ERROR
 * is reached), automatically break the ligature into smaller, valid
 * pieces.
 *
 * TODO: In the future, there will be further ligature engravers
 * implemented, such as a Vaticana_ligature_engraver.  There will be
 * redundant code between these engravers and the
 * Mensural_ligature_engraver.  In particular these are functions
 * set_column_l_, fold_up_primitives, join_primitives, and
 * ackowledge_grob; further the code for handling accidentals.  It is
 * not appropriate to put these things into Ligature_engraver, since,
 * for example, Ligature_bracket_engraver does not share any of this
 * code.  Hence, we might to introduce a further subclass of
 * Ligature_engraver which serves as super class for
 * Mensural_ligature_engraver, Vaticana_ligature_engraver, among
 * others.
 */
class Mensural_ligature_engraver : public Ligature_engraver
{
  Real distance_f_;
  Array<Grob_info> primitives_arr_;

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void try_stop_ligature ();
  virtual Spanner *create_ligature_spanner ();

public:
  TRANSLATOR_DECLARATIONS(Mensural_ligature_engraver);

private:
  int apply_transition (int state, int input, int i);
  void transform_heads ();
  void propagate_properties ();
  void fold_up_primitives ();
  void join_primitives ();
  void set_column_l (Item *item, Paper_column *new_col);
};


Mensural_ligature_engraver::Mensural_ligature_engraver ()
{
  distance_f_ = 0;
}

Spanner *
Mensural_ligature_engraver::create_ligature_spanner ()
{
  distance_f_ = 0;
  return new Spanner (get_property ("MensuralLigature"));
}

/*
 * TODO: move this function to class Item?
 */
void
Mensural_ligature_engraver::set_column_l (Item *item, Paper_column *column)
{
  Item *parent = dynamic_cast<Item*> (item->get_parent (X_AXIS));
  if (!parent)
    {
      programming_error ("failed tweaking paper column in ligature");
      return;
    }

  String name = parent->name ();
  if (!String::compare_i (name, "PaperColumn"))
    {
      // Change column not only for targeted item (NoteColumn), but
      // also for all associated grobs (NoteSpacing, SeparationItem).
      Grob *sl = Staff_symbol_referencer::staff_symbol_l (item);
      for (SCM tail = parent->get_grob_property ("elements");
	   gh_pair_p (tail);
	   tail = ly_cdr (tail))
	{
	  Item *sibling = unsmob_item (ly_car (tail));
	  if ((sibling) &&
	      (Staff_symbol_referencer::staff_symbol_l (sibling) == sl))
	    {
	      sibling->set_parent (column, X_AXIS);
	    }
	}
    }
  else
    {
      set_column_l (parent, column);
    }
}

/*
 * The following lines implement a finite state automat.  Given a
 * sequence of durations (Longa, Brevis, Semibrevis) or
 * end-of-ligature-request as input, the automat outputs a sequence of
 * requests for grobs that form a proper ligature.
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
Mensural_ligature_engraver::apply_transition (int state, int input, int i)
{
  int output = transition_output[state][input];
  Item *last_last_primitive = (i > 1) ?
    dynamic_cast<Item*> (primitives_arr_[i-2].grob_l_) : 0;
  Item *last_primitive = (i > 0) ?
    dynamic_cast<Item*> (primitives_arr_[i-1].grob_l_) : 0;
  Item *primitive = (i < primitives_arr_.size ()) ?
    dynamic_cast<Item*> (primitives_arr_[i].grob_l_) : 0;
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
	last_primitive->set_grob_property ("primitive", gh_int2scm (output));
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
	last_primitive->set_grob_property ("primitive", gh_int2scm (output));
	primitive->set_grob_property ("primitive", gh_int2scm (MLP_NONE));
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
	last_last_primitive->set_grob_property ("primitive", gh_int2scm (output));
	last_primitive->set_grob_property ("primitive", gh_int2scm (MLP_NONE));
	break;
      default:
	programming_error (_f ("unexpected case fall-through"));
	break;
    }
  return transition_state[state][input];
}

void
Mensural_ligature_engraver::transform_heads ()
{
  if (primitives_arr_.size () < 2)
    {
      warning (_f ("ligature with less than 2 heads -> skipping"));
      return;
    }
  int state = STATE_START;
  Pitch last_pitch, pitch;
  bool have_last_pitch = 0, have_pitch = 0;
  for (int i = 0; i < primitives_arr_.size (); i++) {
    last_pitch = pitch;
    have_last_pitch = have_pitch;
    Grob_info info = primitives_arr_[i];
    int duration_log =
      Rhythmic_head::balltype_i (dynamic_cast<Item*> (info.grob_l_));
    Note_req *nr = dynamic_cast<Note_req*> (info.music_cause ());
    if (!nr)
      {
	info.music_cause ()->origin ()->warning (_f ("can not determine pitch of ligature primitive -> skipping"));
	i++;
	state = STATE_START;
	have_pitch = 0;
	continue;
      }
    else
      {
	pitch = *unsmob_pitch (nr->get_mus_property ("pitch"));
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
    state = apply_transition (state, input, i);
    // TODO: if (state == STATE_ERROR) { ... }
  }

  state = apply_transition (state, INPUT_AE, primitives_arr_.size ());
  // TODO: if (state == STATE_ERROR) { ... }
}

void set_delta_pitch (Item *primitive, Grob_info info1, Grob_info info2)
{
  Note_req *nr1 = dynamic_cast<Note_req*> (info1.music_cause ());
  Note_req *nr2 = dynamic_cast<Note_req*> (info2.music_cause ());
  Pitch pitch1 = *unsmob_pitch (nr1->get_mus_property ("pitch"));
  Pitch pitch2 = *unsmob_pitch (nr2->get_mus_property ("pitch"));
  int delta_pitch = (pitch2.steps () - pitch1.steps ());
  primitive->set_grob_property ("delta-pitch", gh_int2scm (delta_pitch));
}

/*
 * A MensuralLigature grob consists of a bunch of LigatureHead grobs
 * that are glued together.  It (a) does make sense to change
 * properties like thickness or flexa-width from one head to the next
 * within a ligature (this would totally screw up alignment), and (b)
 * some of these properties (like flexa-width) are specific to
 * e.g. the MensuralLigature (as in contrast to e.g. LigatureBracket),
 * and therefore should not be handled in the generic LigatureHead
 * (which is also used by LigatureBracket).  Therefore, we let the
 * user control these properties via the concrete Ligature grob (like
 * MensuralLigature) and then copy these properties as necessary to
 * each of the LigatureHead grobs.  This is what
 * propagate_properties() does.
 */
void
Mensural_ligature_engraver::propagate_properties ()
{
  SCM thickness_scm =
    finished_ligature_p_->get_grob_property ("thickness");
  Real thickness = (thickness_scm != SCM_EOL) ?
    gh_scm2double (thickness_scm) : 1.4;
  thickness *= finished_ligature_p_->paper_l ()->get_var ("linethickness");

  /*
   * FIXME: Since character "noteheads--1mensural" is defined in
   * parmesan font only, the right-hand expression in the
   * following assignment evaluates to a width of 0.0, in case
   * font-family of finished_ligature_p_ is _not_ set to "ancient"
   * (by default, it is; see grob properties of MensuralLigature
   * in scm/grob-description.scm).  This may arise severe problems
   * in the future when switching between fonts (e.g. mensural
   * versus neo-mensural).
   */
  Real head_width =
    Font_interface::get_default_font (finished_ligature_p_)->
    find_by_name ("noteheads--1mensural").extent (X_AXIS).length ();
  if (head_width == 0.0)
    {
      programming_error ("Mensural_ligature_engraver: failed evaluating head_width (most probably a font-family selection problem)");
    }

  SCM flexa_width_scm =
    finished_ligature_p_->get_grob_property ("flexa-width");
  Real flexa_width = (flexa_width_scm != SCM_EOL) ?
    gh_scm2double (flexa_width_scm) : 2.0;
  flexa_width *= Staff_symbol_referencer::staff_space (finished_ligature_p_);

  Real half_flexa_width = 0.5 * (flexa_width + thickness);

  for (int i = 0; i < primitives_arr_.size (); i++)
    {
      Item *primitive = dynamic_cast<Item*> (primitives_arr_[i].grob_l_);
      int output = gh_scm2int (primitive->get_grob_property ("primitive"));
      primitive->set_grob_property ("thickness",
				    gh_double2scm (thickness));
      switch (output) {
	case MLP_NONE:
	  primitive->set_grob_property ("head-width",
					gh_double2scm (half_flexa_width));
	  break;
	case MLP_sc:
	case MLP_ss:
	case MLP_cs:
	  primitive->set_grob_property ("head-width",
					gh_double2scm (head_width));
	  break;
	case MLP_BB:
	case MLP_LB:
	case MLP_SS:
	  primitive->set_grob_property ("head-width",
					gh_double2scm (half_flexa_width));
	  primitive->set_grob_property ("flexa-width",
					gh_double2scm (flexa_width));
	  set_delta_pitch (primitive,
			   primitives_arr_[i], primitives_arr_[i+1]);
	  break;
	default:
	  programming_error (_f ("unexpected case fall-through"));
	  break;
      }
    }
}

void
Mensural_ligature_engraver::fold_up_primitives ()
{
  Item *first = 0;
  for (int i = 0; i < primitives_arr_.size (); i++)
    {
      Item *current = dynamic_cast<Item*> (primitives_arr_[i].grob_l_);
      if (i == 0)
	{
	  first = current;
	}

      set_column_l (current, first->column_l ());

      if (i > 0)
	{
#if 0
	  Rod r;
	  r.distance_f_ = distance_f_;
	  r.item_l_drul_[LEFT] = first;
	  r.item_l_drul_[RIGHT] = current;
	  r.add_to_cols ();
#endif
	  current->translate_axis (distance_f_, X_AXIS);
	}

      distance_f_ +=
	gh_scm2double (current->get_grob_property ("head-width")) -
	gh_scm2double (current->get_grob_property ("thickness"));
    }
}

void
Mensural_ligature_engraver::join_primitives ()
{
  Pitch last_pitch;
  for (int i = 0; i < primitives_arr_.size (); i++)
    {
      Grob_info info = primitives_arr_[i];
      Note_req *nr = dynamic_cast<Note_req*> (info.music_cause ());
      Pitch pitch = *unsmob_pitch (nr->get_mus_property ("pitch"));
      if (i > 0)
        {
	  Item *primitive = dynamic_cast<Item*> (info.grob_l_);
	  int output = gh_scm2int (primitive->get_grob_property ("primitive"));
	  if (output & MLP_ANY)
	    {
	      int delta_pitch = (pitch.steps () - last_pitch.steps ());
	      primitive->set_grob_property ("join-left",
					    gh_int2scm (delta_pitch));
	    }
	}
      last_pitch = pitch;
    }
}

void
Mensural_ligature_engraver::try_stop_ligature ()
{
  if (finished_ligature_p_)
    {
      transform_heads ();
      propagate_properties ();
      fold_up_primitives ();
      join_primitives ();

      for (int i = 0; i < primitives_arr_.size (); i++)
	{
	  typeset_grob (primitives_arr_[i].grob_l_);
	}

      primitives_arr_.clear ();
      finished_ligature_p_ = 0;
    }
}

void
Mensural_ligature_engraver::acknowledge_grob (Grob_info info)
{
  Ligature_engraver::acknowledge_grob (info);
  if (ligature_p_)
    {
      if (Note_head::has_interface (info.grob_l_))
	{
	  primitives_arr_.push (info);
	}
    }
}

ENTER_DESCRIPTION(Mensural_ligature_engraver,
/* descr */       "Handles Mensural_ligature_requests by glueing special ligature heads together.",
/* creats*/       "MensuralLigature",
/* acks  */       "ligature-head-interface note-head-interface rest-interface",
/* reads */       "",
/* write */       "");
