/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

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


Score::Score()
    : Input()
{
  header_p_ = 0;
  music_p_ = 0;
  errorlevel_i_ = 0;
}

Score::Score (Score const &s)
  : Input (s)
{
  music_p_ = (s.music_p_) ? s.music_p_->clone() : 0;
  for (int i=0; i < s.def_p_arr_.size (); i++)
    def_p_arr_.push(s.def_p_arr_[i]->clone());

  header_p_ =  (s.header_p_) ? new Header (*s.header_p_): 0;
}

Score::~Score()
{
  delete header_p_;
  for (int i=0; i < def_p_arr_.size (); i++)
    delete def_p_arr_[i];
  delete music_p_;
}

void
Score::run_translator (Music_output_def *odef_l)
{
  Cpu_timer timer;
  Global_translator * trans_p = odef_l->get_global_translator_p();
  if (!trans_p)
    {
      non_fatal_error (_("no toplevel translator"));
      return ;
    }
  *mlog << '\n' << _("Interpreting music...") << flush;
  trans_p->last_mom_ = music_p_->duration ();

  Music_iterator * iter = Music_iterator::static_get_iterator_p (music_p_, trans_p);
  

  iter->construct_children();

  if (! iter->ok())
    {
      delete iter;
      warning (_("need music in a score"));
      errorlevel_i_ =1;
      return ;
    }

  trans_p->start();

  while (iter->ok() || trans_p->moments_left_i ())
    {
      Moment w;
      w.set_infinite (1);
      if (iter->ok())
	{
	  w = iter->next_moment();
	  DOUT << "proccing: " << w << '\n';
	  if (!monitor->silent_b ("walking"))
	    iter->print();
	}
      
      trans_p->modify_next (w);
      trans_p->prepare (w);
      
      if (!monitor->silent_b ("walking"))
	trans_p->print();

      iter->process_and_next (w);
      trans_p->process();
    }
  
  delete iter;
  trans_p->finish();


  if (errorlevel_i_)
    {
      // should we? hampers debugging.
      warning (_ ("errors found, /*not processing score*/"));
    }

  Music_output * output = trans_p->get_output_p();
  delete trans_p;
  *mlog << endl << _f ("time: %.2f seconds",  timer.read ()) << flush;

  output->header_l_ = header_p_;
  output->origin_str_ =  location_str();

  *mlog << endl;
  output->process();
  delete output ;
}

void
Score::process()
{
  if (!music_p_)
    return;

  print();
  for (int i=0; i < def_p_arr_.size (); i++)
    {
      if (no_paper_global_b 
	  && def_p_arr_[i]->is_type_b (Paper_def::static_name ()))
	continue;
      run_translator (def_p_arr_[i]);
    }
}



void
Score::print() const
{
#ifndef NPRINT
  DOUT << "score {\n";
  music_p_ -> print ();
  for (int i=0; i < def_p_arr_.size (); i++)
    def_p_arr_[i]->print();
  DOUT << "}\n";
#endif
}

void
Score::add_output (Music_output_def *pap_p)
{
  def_p_arr_.push(pap_p);
}
