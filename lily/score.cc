/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "ly-smobs.icc"

#include "scm-hash.hh"
#include "score.hh"
#include "debug.hh"
#include "music-output-def.hh"
#include "music-output.hh"
#include "music-iterator.hh"
#include "music.hh"
#include "global-translator.hh"
#include "scope.hh"
#include "cpu-timer.hh"
#include "main.hh"
#include "paper-def.hh"


/*
  TODO: junkme.
 */

Score::Score ()
  : Input ()
{
  header_p_ = 0;
  music_ = SCM_EOL;
  errorlevel_i_ = 0;
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
  header_p_ = 0;
  smobify_self ();

  /*
    TODO: this is not very elegant.... 
   */
  store_locations_global_b = (gh_eval_str ("point-and-click") !=  SCM_BOOL_F);

  Music * m =unsmob_music (s.music_);
  music_ =  m?m->clone ()->self_scm () : SCM_EOL;
  scm_gc_unprotect_object (music_);
  
  for (int i=0; i < s.def_p_arr_.size (); i++)
    def_p_arr_.push (s.def_p_arr_[i]->clone ());
  errorlevel_i_ = s.errorlevel_i_;
  if (s.header_p_)
    {
      header_p_ = (s.header_p_) ? new Scheme_hash_table (*s.header_p_): 0;

      scm_gc_unprotect_object (header_p_->self_scm ());
    }
}

Score::~Score ()
{
  
}

void
Score::run_translator (Music_output_def *odef_l)
{
  Cpu_timer timer;

  
  Global_translator * trans_p = odef_l->get_global_translator_p ();
  if (!trans_p)
    {
      programming_error ("no toplevel translator");
      return ;
    }
  progress_indication (_ ("Interpreting music..."));
  Music * music = unsmob_music (music_);
  
  trans_p->final_mom_ = music->length_mom ();


  Music_iterator * iter = Music_iterator::static_get_iterator_p (music);
  iter->init_translator (music, trans_p);

  iter->construct_children ();

  if (! iter->ok ())
    {
      delete iter;
      warning (_ ("Need music in a score"));
      errorlevel_i_ =1;
      return ;
    }

  trans_p->start ();
  trans_p->run_iterator_on_me (iter);
  delete iter;
  trans_p->finish ();

  if (errorlevel_i_)
    {
      // should we? hampers debugging.
      warning (_ ("Errors found/*, not processing score*/"));
    }

  Music_output * output = trans_p->get_output_p ();
  scm_gc_unprotect_object (trans_p->self_scm ());
  
  if (verbose_global_b)
    progress_indication (_f ("elapsed time: %.2f seconds",  timer.read ()));

  if (!header_p_)
    header_p_ = new Scheme_hash_table; // ugh
  Scope bla (header_p_);
  output->header_l_ = &bla;
  output->origin_str_ =  location_str ();

  progress_indication ("\n");
  output->process ();
  delete output ;

  /*
    force GC. At this point, GUILE may give back mallocated area to
    the system.
  */
    
  scm_gc ();
}

void
Score::process ()
{
  if (!unsmob_music (music_))
    return;


  for (int i=0; i < def_p_arr_.size (); i++)
    {
      if (no_paper_global_b 
	  && dynamic_cast<Paper_def*> (def_p_arr_[i]))
	continue;
      run_translator (def_p_arr_[i]);
    }
}




void
Score::add_output (Music_output_def *pap_p)
{
  def_p_arr_.push (pap_p);
}

IMPLEMENT_SMOBS (Score);
IMPLEMENT_DEFAULT_EQUAL_P (Score);
IMPLEMENT_UNSMOB (Score, score);

SCM
Score::mark_smob (SCM s)
{
  Score * sc = (Score*) SCM_CELL_WORD_1 (s);
  if (sc->header_p_)
    scm_gc_mark (sc->header_p_->self_scm ());
  for (int i = sc->def_p_arr_.size (); i--;)
    scm_gc_mark (sc->def_p_arr_[i]->self_scm ());
  
  return sc->music_;
}

int
Score::print_smob (SCM s, SCM p, scm_print_state*)
{
  scm_puts ("#<Score>", p);

  return 1;
}
