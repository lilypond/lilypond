/*
  ligature-engraver.cc -- implement Ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2005 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "ligature-engraver.hh"

#include "spanner.hh"
#include "score-engraver.hh"
#include "note-head.hh"
#include "rest.hh"
#include "warn.hh"
#include "context.hh"

/*
 * This abstract class provides the general framework for ligatures of
 * any kind.  It cares for handling start/stop ligatures events and
 * collecting all noteheads inbetween, but delegates creation of a
 * ligature spanner for each start/stop pair and typesetting of the
 * ligature spanner to a concrete subclass.
 *
 * A concrete ligature engraver must subclass this class and provide
 * functions create_ligature_spanner () and typeset_ligature
 * (Spanner *, Array<Grob_info>).  Subclasses of this class basically
 * fall into two categories.
 *
 * The first category consists of engravers that engrave ligatures in
 * a way that really deserves the name ligature.  That is, they
 * produce a single connected graphical object of fixed width,
 * consisting of noteheads and other primitives.  Space may be
 * inserted only after each ligature, if necessary, but in no case
 * between the primitives of the ligature.  Accidentals have to be put
 * to the left of the ligature, and not to the left of individual
 * noteheads.  Class Coherent_ligature_engraver is the common
 * superclass for all of these engravers.
 *
 * The second category is for engravers that are relaxed in the sense
 * that they do not require to produce a single connected graphical
 * object.  For example, in contemporary editions, ligatures are often
 * marked, but otherwise use contemporary notation and spacing.  In
 * this category, there is currently only a single class,
 * Ligature_bracket_engraver, which marks each ligature with a
 * horizontal sqare bracket, but otherwise leaves the appearance
 * untouched.
 */

/*
 * TODO: lyrics/melisma/syllables: there should be at most one
 * syllable of lyrics per ligature (i.e. for the lyrics context, a
 * ligature should count as a single note, regardless of how many
 * heads the ligature consists of).
 *
 * TODO: currently, you have to add/remove the proper
 * Ligature_engraver (Ligature_bracket_engraver,
 * Mensural_ligature_engraver) to the proper translator
 * (e.g. VoiceContext) to choose between various representations.
 * Since adding/removing an engraver to a translator is a global
 * action in the layout block, you cannot mix various representations
 * _within_ the same score.  Hence, for selecting a representation,
 * one would rather like to have a property that can be set e.g. for
 * several staves individually.  However, it seems that this approach
 * would require to have a single, complicated Ligature_engraver that
 * consists of all the code...  This needs further thoughts.
 */
Ligature_engraver::Ligature_engraver ()
{
  ligature_ = 0;
  finished_ligature_ = 0;
  events_drul_[LEFT] = events_drul_[RIGHT] = 0;
  prev_start_event_ = 0;
  last_bound_ = 0;
  brew_ligature_primitive_proc = SCM_EOL;
}

bool
Ligature_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("ligature-event"))
    {
      Direction d = to_dir (m->get_property ("span-direction"));
      events_drul_[d] = m;
      return true;
    }
  return false;
}

Spanner *
Ligature_engraver::create_ligature_spanner ()
{
  programming_error ("Ligature_engraver::create_ligature_spanner (): "
		     "this is an abstract method that should not be called, "
		     "but overridden by a subclass");
  return 0;
}

/*
 * This method should do something that comes close to the following
 * .ly snippet:
 *
 * \property Voice.NoteHead \override #'print-function =
 *     < value of #'ligature-primitive-callback of Voice.NoteHead >
 *
 * TODO: What we are doing here on the c++ level, should actually be
 * performed on the SCM level.  However, I do not know how to teach
 * lilypond to apply an \override and \revert on #'print-function,
 * whenever lily encounters a \[ and \] in an .ly file, respectively.
 * Also encounter, that lily should not crash if a user erronously
 * nests \[ and \].
 */
void
Ligature_engraver::override_stencil_callback ()
{
  SCM target_callback = ly_symbol2scm ("print-function");
  SCM source_callback = ly_symbol2scm ("ligature-primitive-callback");
  SCM noteHeadProperties = updated_grob_properties (context (), ly_symbol2scm ("NoteHead"));
  SCM value = scm_cdr (scm_sloppy_assq (source_callback, noteHeadProperties));
  execute_pushpop_property (context (), ly_symbol2scm ("NoteHead"),
			    target_callback, value);
}

/*
 * This method should do something that comes close to the following
 * .ly snippet:
 *
 * \property Voice.NoteHead \revert #'print-function
 *
 * TODO: What we are doing here on the c++ level, should actually be
 * performed on the SCM level.  However, I do not know how to teach
 * lilypond to apply an \override and \revert on #'print-function,
 * whenever lily encounters a \[ and \] in an .ly file, respectively.
 * Also encounter, that lily should not crash if a user erronously
 * nests \[ and \].
 */
void
Ligature_engraver::revert_stencil_callback ()
{
  SCM symbol = ly_symbol2scm ("NoteHead");
  SCM key = ly_symbol2scm ("print-function");
  execute_pushpop_property (context (), symbol, key, SCM_UNDEFINED);
}

void
Ligature_engraver::process_music ()
{
  if (events_drul_[STOP])
    {
      if (!ligature_)
	{
	  events_drul_[STOP]->origin ()->warning (_ ("can't find start of ligature"));
	  return;
	}

      if (!last_bound_)
	{
	  events_drul_[STOP]->origin ()->warning (_ ("no right bound"));
	}
      else
	{
	  ligature_->set_bound (RIGHT, last_bound_);
	}

      prev_start_event_ = 0;
      finished_primitives_ = primitives_;
      finished_ligature_ = ligature_;
      primitives_.clear ();
      ligature_ = 0;
      revert_stencil_callback ();
    }
  last_bound_ = unsmob_grob (get_property ("currentMusicalColumn"));

  if (ligature_)
    {
      // TODO: maybe forbid breaks only if not transcribing
      get_score_engraver ()->forbid_breaks ();
    }

  if (events_drul_[START])
    {
      if (ligature_)
	{
	  events_drul_[START]->origin ()->warning (_ ("already have a ligature"));
	  return;
	}

      prev_start_event_ = events_drul_[START];
      ligature_ = create_ligature_spanner ();
      brew_ligature_primitive_proc
	= ligature_->get_property ("ligature-primitive-callback");
      if (brew_ligature_primitive_proc == SCM_EOL)
	{
	  programming_error ("Ligature_engraver: ligature-primitive-callback undefined");
	}

      Grob *bound = unsmob_grob (get_property ("currentMusicalColumn"));
      if (!bound)
	{
	  events_drul_[START]->origin ()->warning (_ ("no left bound"));
	}
      else
	{
	  ligature_->set_bound (LEFT, bound);
	}

      ligature_start_mom_ = now_mom ();

      // TODO: dump cause into make_item/spanner. 
      // announce_grob (ligature_, events_drul_[START]->self_scm ());
      override_stencil_callback ();
    }
}

void
Ligature_engraver::typeset_ligature (Spanner *, Array<Grob_info>)
{
  programming_error ("Ligature_engraver::typeset_ligature (): "
		     "this is an abstract method that should not be called, "
		     "but overridden by a subclass");
}

void
Ligature_engraver::stop_translation_timestep ()
{
  if (finished_ligature_)
    {
      if (!finished_primitives_.size ())
	{
	  finished_ligature_->programming_error ("Ligature_engraver::stop_translation_timestep (): "
						 "junking empty ligature");
	}
      else
	{
	  typeset_ligature (finished_ligature_, finished_primitives_);
	  finished_primitives_.clear ();
	}
      finished_ligature_ = 0;
    }

  events_drul_[START] = 0;
  events_drul_[STOP] = 0;
}

void
Ligature_engraver::finalize ()
{
  if (finished_ligature_)
    {
      typeset_ligature (finished_ligature_, finished_primitives_);
      finished_primitives_.clear ();
      finished_ligature_ = 0;
    }
  if (ligature_)
    {
      prev_start_event_->origin ()->warning (_ ("unterminated ligature"));
      ligature_->suicide ();
    }
}

Spanner *
Ligature_engraver::current_ligature ()
{
  return ligature_;
}

void
Ligature_engraver::acknowledge_note_head (Grob_info info)
{
  if (ligature_)
    {
      primitives_.push (info);
      info.grob ()->set_property ("print-function",
				  brew_ligature_primitive_proc);
    }
}

void
Ligature_engraver::acknowledge_rest (Grob_info info)
{
  info.music_cause ()->origin ()->warning (_ ("ignoring rest: ligature may not contain rest"));
  prev_start_event_->origin ()->warning (_ ("ligature was started here"));
  // TODO: maybe better should stop ligature here rather than
  // ignoring the rest?
}


#include "translator.icc"

ADD_ACKNOWLEDGER(Ligature_engraver, rest);
ADD_ACKNOWLEDGER(Ligature_engraver, note_head);
ADD_TRANSLATOR (Ligature_engraver,
		/* descr */ "Abstract class; a concrete subclass handles Ligature_events by engraving Ligatures in a concrete style.",
		/* creats */ "",
		/* accepts */ "ligature-event",
		/* acks  */ "",
		/* reads */ "",
		/* write */ "");
