/*
  recording-group-engraver.cc -- implement Recording_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "recording-group-engraver.hh"
#include "context.hh"

void
Recording_group_engraver::derived_mark () const
{
  Engraver_group::derived_mark ();
  scm_gc_mark (now_events_);
  scm_gc_mark (accumulator_);
}

Recording_group_engraver::Recording_group_engraver ()
{
  accumulator_ = SCM_EOL;
  now_events_ = SCM_EOL;
}

void
Recording_group_engraver::add_music (SCM music, SCM success)
{
  now_events_ = scm_cons (scm_cons (music, success), now_events_);
}

void
Recording_group_engraver::stop_translation_timestep ()
{
  accumulator_ = scm_acons (scm_cons (context ()->now_mom ().smobbed_copy (),
				      context ()->get_property ("instrumentTransposition")),
			    now_events_,
			    accumulator_);
  now_events_ = SCM_EOL;
}

void
Recording_group_engraver::finalize ()
{
  SCM proc = context ()->get_property ("recordEventSequence");

  if (ly_is_procedure (proc))
    scm_call_2 (proc, context ()->self_scm (), scm_cdr (accumulator_));
}

bool
Recording_group_engraver::try_music (Music *m)
{
  bool retval = Translator_group::try_music (m);

  add_music (m->self_scm (), ly_bool2scm (retval));
  return retval;
}

void
recording_engraver (Translator_group *tg)
{
  Recording_group_engraver *rg = dynamic_cast<Recording_group_engraver *> (tg);
  rg->stop_translation_timestep ();
}

void
Recording_group_engraver::fetch_precomputable_methods (Translator_group_void_method ptrs[])
{
  Translator_group::fetch_precomputable_methods (ptrs);
  ptrs[STOP_TRANSLATION_TIMESTEP] = &recording_engraver;
}

ADD_TRANSLATOR_GROUP (Recording_group_engraver,
		      "Engraver_group that records all music events "
		      "for this context. Calls the procedure "
		      "in @code{recordEventSequence} when finished.",
		      "",
		      "",
		      "recordEventSequence",
		      "");
