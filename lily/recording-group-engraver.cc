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
  void start ();
public:
  TRANSLATOR_DECLARATIONS (Recording_group_engraver);
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
  start ();
}

Recording_group_engraver::Recording_group_engraver ()
{
}

void
Recording_group_engraver::start_translation_timestep ()
{
  Engraver_group_engraver::start_translation_timestep ();


  /*
    We have to do this both in initialize () and
    start_translation_timestep (), since start_translation_timestep ()
    isn't called on the first time-step.
   */
  start () ;
}

void
Recording_group_engraver::start ()
{
  if (!is_pair (accumulator_))
    accumulator_ = scm_cons (SCM_EOL, SCM_EOL);
  if (!is_pair (ly_car (accumulator_)))
    {
      /*
	Need to store transposition for every moment; transposition changes during pieces.
       */
      scm_set_car_x (accumulator_, scm_cons (scm_cons (now_mom ().smobbed_copy (),
						     get_property ("instrumentTransposition")),
						     SCM_EOL));
    }
}

void
Recording_group_engraver::stop_translation_timestep ()
{
  Engraver_group_engraver::stop_translation_timestep ();
  scm_set_cdr_x (accumulator_, scm_cons (ly_car (accumulator_), ly_cdr (accumulator_)));

  scm_set_car_x (accumulator_, SCM_EOL);
}

void
Recording_group_engraver::finalize ()
{
  Engraver_group_engraver::finalize ();
  SCM proc = get_property ("recordEventSequence");

  if (is_procedure (proc))
    scm_call_2  (proc, get_parent_context ()->self_scm (), ly_cdr (accumulator_));

  accumulator_ = SCM_EOL;
}

bool
Recording_group_engraver::try_music (Music  *m)
{
  bool retval = Translator_group::try_music (m);

  SCM seq = ly_cdar (accumulator_);
  seq = scm_cons (scm_cons  (m->self_scm (), ly_bool2scm (retval)),
		 seq);
  
  scm_set_cdr_x  (ly_car (accumulator_), seq);

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
