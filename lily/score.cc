/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#include "ly-smobs.icc"

#include "score.hh"
#include "warn.hh"
#include "music-output-def.hh"
#include "music-output.hh"
#include "music-iterator.hh"
#include "music.hh"
#include "global-translator.hh"
#include "scm-hash.hh"
#include "cpu-timer.hh"
#include "main.hh"
#include "paper-def.hh"


/*
  TODO: junkme.
 */
Score::Score ()
  : Input ()
{
  input_file_ = 0;
  header_ = 0;
  music_ = SCM_EOL;
  errorlevel_ = 0;

  smobify_self ();
}

/*
  store point & click locations.
  Global to save some time. (Sue us!)
 */
bool store_locations_global_b;

Score::Score (Score const &s)
  : Input (s)
{
  music_ = SCM_EOL;
  header_ = 0;
  smobify_self ();

  Music * m =unsmob_music (s.music_);
  music_ =  m?m->clone ()->self_scm () : SCM_EOL;
  scm_gc_unprotect_object (music_);
  
  for (int i=0; i < s.defs_.size (); i++)
    defs_.push (s.defs_[i]->clone ());
  errorlevel_ = s.errorlevel_;
  if (s.header_)
    {
      header_ = (s.header_) ? new Scheme_hash_table (*s.header_): 0;

      scm_gc_unprotect_object (header_->self_scm ());
    }
}

Score::~Score ()
{
  
}



void
Score::run_translator (Music_output_def *odef)
{
  /*
    We want to know if we want to store locations, since they take a
    lot of overhead.
  */
  store_locations_global_b = (gh_eval_str ("point-and-click") !=  SCM_BOOL_F);
  
  Cpu_timer timer;
  Global_translator * trans = odef->get_global_translator ();
  if (!trans)
    {
      programming_error ("no toplevel translator");
      return ;
    }
  progress_indication (_ ("Interpreting music..."));
  Music * music = unsmob_music (music_);
  
  trans->final_mom_ = music->length_mom ();
  SCM protected_iter =  Music_iterator::get_static_get_iterator (music);
  Music_iterator * iter = unsmob_iterator (protected_iter);
  iter->init_translator (music, trans);

  iter->construct_children ();

  if (! iter->ok ())
    {
      warning (_ ("Need music in a score"));
      errorlevel_ =1;
      return ;
    }

  trans->start ();
  trans->run_iterator_on_me (iter);
  scm_remember_upto_here_1 (protected_iter);
  trans->finish ();

  if (errorlevel_)
    {
      // should we? hampers debugging.
      warning (_ ("Errors found/*, not processing score*/"));
    }

  Music_output * output = trans->get_output ();
  scm_gc_unprotect_object (trans->self_scm ());
  
  if (verbose_global_b)
    progress_indication (_f ("elapsed time: %.2f seconds",  timer.read ()));

  if (!header_)
    header_ = new Scheme_hash_table; // ugh

  output->header_ = header_;
  output->origin_string_ =  location_string ();

  progress_indication ("\n");
  output->process ();
  
  delete output ;
}

void
Score::process ()
{
  if (!unsmob_music (music_))
    return;

  for (int i=0; i < defs_.size (); i++)
    {
      if (no_paper_global_b 
	  && dynamic_cast<Paper_def*> (defs_[i]))
	continue;
      run_translator (defs_[i]);
    }
}


void
Score::add_output (Music_output_def *pap)
{
  defs_.push (pap);
}

IMPLEMENT_SMOBS (Score);
IMPLEMENT_DEFAULT_EQUAL_P (Score);


SCM
Score::mark_smob (SCM s)
{
  Score * sc = (Score*) SCM_CELL_WORD_1 (s);
  if (sc->header_)
    scm_gc_mark (sc->header_->self_scm ());
  for (int i = sc->defs_.size (); i--;)
    scm_gc_mark (sc->defs_[i]->self_scm ());
  
  return sc->music_;
}

int
Score::print_smob (SCM , SCM p, scm_print_state*)
{
  scm_puts ("#<Score>", p);

  return 1;
}
