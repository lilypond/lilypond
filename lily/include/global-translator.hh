/*
  global-translator.hh -- declare Global_translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef GLOBAL_TRANSLATOR_HH
#define GLOBAL_TRANSLATOR_HH

#include "translator-group.hh"
#include "moment.hh"
#include "pqueue.hh"

class Global_translator : public virtual Translator_group
{
  PQueue<Moment> extra_mom_pq_;
  Music_output_def*  output_def_;
  friend class Music_output_def;
public:
  VIRTUAL_COPY_CONS (Translator);
  Global_translator ();

  int get_moments_left () const;
  Moment sneaky_insert_extra_moment (Moment);
  void add_moment_to_process (Moment);
  void run_iterator_on_me (Music_iterator*);

  void apply_finalizations ();
  void add_finalization (SCM);
  
  virtual Music_output *get_output ();     
  virtual void prepare (Moment);
  virtual void one_time_step ();
  virtual void finish ();
  virtual void start ();
  virtual Music_output_def* get_output_def () const; 
  virtual Moment now_mom () const;

  Moment final_mom_;
  Moment prev_mom_;
  Moment now_mom_;
protected:
};



#endif // GLOBAL_TRANSLATOR_HH
