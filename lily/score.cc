/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <iostream.h>

#include "score.hh"
#include "debug.hh"
#include "music-output-def.hh"
#include "music-output.hh"
#include "source.hh"
#include "source-file.hh"
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

Score::Score()
  : Input()
{
  header_p_ = 0;
  music_ = SCM_EOL;
  errorlevel_i_ = 0;
}

Score::Score (Score const &s)
  : Input (s)
{
  Music * m =unsmob_music (s.music_);
  music_ =  m?m->clone()->self_scm () : SCM_EOL;
  for (int i=0; i < s.def_p_arr_.size (); i++)
    def_p_arr_.push(s.def_p_arr_[i]->clone());
  errorlevel_i_ = s.errorlevel_i_;
  header_p_ =  (s.header_p_) ? new Scope (*s.header_p_): 0;
}

Score::~Score()
{
  delete header_p_;
  junk_pointer_array (def_p_arr_);
}

void
Score::run_translator (Music_output_def *odef_l)
{
  Cpu_timer timer;

  
  Global_translator * trans_p = odef_l->get_global_translator_p();
  if (!trans_p)
    {
      programming_error ("no toplevel translator");
      return ;
    }
  progress_indication ("\n" + _("Interpreting music..."));
  Music * music = unsmob_music (music_);
  
  trans_p->final_mom_ = music->length_mom ();


  Music_iterator * iter = Music_iterator::static_get_iterator_p (music);
  iter->init_translator(music, trans_p);

  iter->construct_children();

  if (! iter->ok())
    {
      delete iter;
      warning (_("Need music in a score"));
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

  Music_output * output = trans_p->get_output_p();
  delete trans_p;
  if(verbose_global_b)
    progress_indication (_f ("elapsed time: %.2f seconds",  timer.read ()));

  output->header_l_ = header_p_;
  output->origin_str_ =  location_str();

  progress_indication ("\n");
  output->process();
  delete output ;

  /*
    force GC. At this point, GUILE may give back mallocated area to
    the system.
  */
    
  scm_gc();
}

void
Score::process()
{
  if (!unsmob_music (music_))
    return;

  print();
  for (int i=0; i < def_p_arr_.size (); i++)
    {
      if (no_paper_global_b 
	  && dynamic_cast<Paper_def*>(def_p_arr_[i]))
	continue;
      run_translator (def_p_arr_[i]);
    }
}



void
Score::print() const
{
#ifndef NPRINT
  DEBUG_OUT << "score {\n";
  // music_p_ -> print ();
  for (int i=0; i < def_p_arr_.size (); i++)
    def_p_arr_[i]->print();
  DEBUG_OUT << "}\n";
#endif
}

void
Score::add_output (Music_output_def *pap_p)
{
  def_p_arr_.push(pap_p);
}
