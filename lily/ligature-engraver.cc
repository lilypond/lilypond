/*   
  ligature-engraver.cc -- implement Ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
  
 */
#include "ligature-engraver.hh"
#include "ligature-head.hh"
#include "spanner.hh"
#include "score-engraver.hh"
#include "rest.hh"
#include "warn.hh"

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
 * action in the paper block, you can not mix various representations
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
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] = 0;
  prev_start_req_ = 0;
  last_bound = 0;
  brew_ligature_primitive_proc = SCM_EOL;
}

bool
Ligature_engraver::try_music (Music *m)
{
  if (Span_req *req_ = dynamic_cast<Span_req*> (m))
    {
      if (scm_equal_p (req_->get_mus_property ("span-type"),
		       scm_makfrom0str ("abort")) == SCM_BOOL_T)
	{
	  reqs_drul_[START] = 0;
	  reqs_drul_[STOP] = 0;
	  if (ligature_)
	    ligature_->suicide ();
	  ligature_ = 0;
	}
      else if (scm_equal_p (req_->get_mus_property ("span-type"),
			    scm_makfrom0str ("ligature")) == SCM_BOOL_T)
	{
	  Direction d = req_->get_span_dir ();
	  reqs_drul_[d] = req_;
	  return true;
	}
    }
  return false;
}

Spanner *
Ligature_engraver::create_ligature_spanner ()
{
  return new Spanner (SCM_EOL);
}

void
Ligature_engraver::process_music ()
{
  if (reqs_drul_[STOP])
    {
      if (!ligature_)
	reqs_drul_[STOP]->origin ()->warning (_ ("can't find start of ligature"));
      else
	{
	  if (!last_bound)
	    {
	      reqs_drul_[STOP]->origin ()->warning (_ ("no right bound"));
	    }
	  else
	    {
	      ligature_->set_bound (RIGHT, last_bound);
	    }
	}
      prev_start_req_ = 0;
      finished_ligature_ = ligature_;
      ligature_ = 0;
    }
  last_bound = unsmob_grob (get_property ("currentMusicalColumn"));

  if (ligature_)
    {
      // TODO: maybe forbid breaks only if not transcribing
      top_engraver ()->forbid_breaks ();
    }
  if (reqs_drul_[START])
    {
      if (ligature_)
	{
	  reqs_drul_[START]->origin ()->warning (_ ("already have a ligature"));
	  return;
	}

      prev_start_req_ = reqs_drul_[START];
      ligature_ = create_ligature_spanner ();
      brew_ligature_primitive_proc =
	ligature_->get_grob_property ("ligature-primitive-callback");
      if (brew_ligature_primitive_proc == SCM_EOL)
	{
	  warning ("Ligature_engraver: ligature-primitive-callback undefined");
	}

      Grob *bound = unsmob_grob (get_property ("currentMusicalColumn"));
      if (!bound)
	{
	  reqs_drul_[START]->origin ()->warning (_ ("no left bound"));
	}
      else
	{
	  ligature_->set_bound (LEFT, bound);
	}

      ligature_start_mom_ = now_mom ();
      
      announce_grob(ligature_, reqs_drul_[START]->self_scm());
    }
}

void
Ligature_engraver::start_translation_timestep ()
{
  reqs_drul_[START] = 0;
  reqs_drul_[STOP] = 0;
}

void
Ligature_engraver::try_stop_ligature ()
{
  if (finished_ligature_)
    {
      typeset_grob (finished_ligature_);
      finished_ligature_ = 0;
    }
}

void
Ligature_engraver::stop_translation_timestep ()
{
  try_stop_ligature ();
}

void
Ligature_engraver::finalize ()
{
  try_stop_ligature ();
  if (ligature_)
    {
      prev_start_req_->origin ()->warning (_ ("unterminated ligature"));
      ligature_->suicide ();
    }
}

void
Ligature_engraver::acknowledge_grob (Grob_info info)
{
  if (ligature_)
    {
      if (Ligature_head::has_interface (info.grob_))
	{
	  info.grob_->set_grob_property ("ligature-primitive-callback",
					   brew_ligature_primitive_proc);
	}
      else if (Rest::has_interface (info.grob_))
	{
	  info.music_cause ()->origin ()->warning (_ ("ligature may not contain rest; ignoring rest"));
	  prev_start_req_->origin ()->warning (_ ("ligature was started here"));
	  // TODO: maybe better should stop ligature here rather than
	  // ignoring the rest?
	}
    }
}

ENTER_DESCRIPTION(Ligature_engraver,
/* descr */       "Abstract class; a concrete subclass handles Ligature_requests by engraving Ligatures in a concrete style.",
/* creats*/       "Ligature",
/* acks  */       "ligature-head-interface rest-interface",
/* reads */       "",
/* write */       "");
