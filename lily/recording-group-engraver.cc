/*   
  recording-group-engraver.cc -- implement Recording_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */

#include "engraver-group-engraver.hh"
#include "protected-scm.hh"

class Recording_group_engraver : public Engraver_group_engraver
{
public:
  TRANSLATOR_DECLARATIONS(Recording_group_engraver);
  virtual bool try_music (Music *m);
  virtual void start_translation_timestep ();
  virtual void stop_translation_timestep ();
  virtual void finalize ();
  virtual void initialize ();
  Protected_scm accumulator_;
};

void
Recording_group_engraver::initialize ()
{
  Engraver_group_engraver::initialize ();
  accumulator_ = gh_cons (SCM_EOL, SCM_EOL);
}

Recording_group_engraver::Recording_group_engraver()
{
}

void
Recording_group_engraver::start_translation_timestep ()
{
  Engraver_group_engraver::start_translation_timestep();
  
  scm_set_car_x (accumulator_, gh_cons (now_mom ().smobbed_copy (), SCM_EOL));
}

void
Recording_group_engraver::stop_translation_timestep ()
{
  Engraver_group_engraver::stop_translation_timestep();
  scm_set_cdr_x (accumulator_, gh_cons (gh_car (accumulator_), gh_cdr (accumulator_)));

  scm_set_car_x (accumulator_, SCM_EOL);
}

void
Recording_group_engraver::finalize ()
{
  Engraver_group_engraver::finalize ();
  SCM proc = get_property ("recordEventSequence");

  if (gh_procedure_p (proc))
    scm_call_2  (proc, self_scm(), gh_cdr (accumulator_));

  accumulator_ = SCM_EOL;
}

bool
Recording_group_engraver::try_music (Music  *m)
{
  bool here_success = Translator_group::try_music_on_nongroup_children (m);
  bool retval = here_success;
  if (!here_success && daddy_trans_)
    retval = daddy_trans_->try_music (m);
      
  SCM seq = gh_cdar (accumulator_);
  seq = gh_cons (gh_cons  (m->self_scm(), gh_bool2scm (here_success)),
		 seq);
  
  scm_set_cdr_x  (gh_car (accumulator_), seq);

  return retval;
}


ENTER_DESCRIPTION(Recording_group_engraver,
		  "Engraver_group_engraver that records all music events "
		  "for this context. Calls the procedure "
		  "in @code{recordEventSequence} when finished.",
		  "",
		  "",
		  "",
		  "recordEventSequence",
		  "");
