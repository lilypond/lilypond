/*   
  recording-group-engraver.cc -- implement Recording_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */

#include "context.hh"
#include "engraver-group-engraver.hh"
#include "protected-scm.hh"

class Recording_group_engraver : public Engraver_group_engraver
{
public:
  TRANSLATOR_DECLARATIONS (Recording_group_engraver);
  virtual bool try_music (Music *m);
  void add_music (SCM, SCM);
  virtual void stop_translation_timestep ();
  virtual void finalize ();
  virtual void initialize ();
  virtual void derived_mark () const;
  SCM now_events_;
  SCM accumulator_;
};

void
Recording_group_engraver::derived_mark () const
{
  Engraver_group_engraver::derived_mark();
  scm_gc_mark (now_events_);
  scm_gc_mark (accumulator_);
}

void
Recording_group_engraver::initialize ()
{
  Engraver_group_engraver::initialize ();
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
  Engraver_group_engraver::stop_translation_timestep ();

  accumulator_ = scm_acons (scm_cons (now_mom ().smobbed_copy (),
				     get_property ("instrumentTransposition")),
			    now_events_,
			    accumulator_);
  now_events_ = SCM_EOL;
}

void
Recording_group_engraver::finalize ()
{
  Engraver_group_engraver::finalize ();
  SCM proc = get_property ("recordEventSequence");

  if (ly_c_procedure_p (proc))
    scm_call_2  (proc, context ()->self_scm (), scm_cdr (accumulator_));
}

bool
Recording_group_engraver::try_music (Music  *m)
{
  bool retval = Translator_group::try_music (m);

  add_music (m->self_scm (), ly_bool2scm (retval));
  return retval;
}


ENTER_DESCRIPTION (Recording_group_engraver,
		  "Engraver_group_engraver that records all music events "
		  "for this context. Calls the procedure "
		  "in @code{recordEventSequence} when finished.",
		  "",
		  "",
		  "",
		  "recordEventSequence",
		  "");
