/*
  global-translator.hh -- declare Global_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef GLOBAL_TRANSLATOR_HH
#define GLOBAL_TRANSLATOR_HH

#include "translator-group.hh"
#include "moment.hh"
#include "pqueue.hh"


class Global_translator : public virtual Translator_group{
  PQueue<Moment> extra_mom_pq_;
public:
  VIRTUAL_COPY_CONS(Translator);
  Moment final_mom_;
  Moment prev_mom_;
  Moment now_mom_;
  Global_translator();

  int moments_left_i() const;
  void modify_next (Moment&);
  void add_moment_to_process (Moment);
  void run_iterator_on_me (Music_iterator*);
  
  virtual Music_output *get_output_p ();     
  virtual void prepare (Moment);
  virtual void process();
  virtual void finish();
  virtual void start();

  virtual Moment now_mom () const;

  
protected:

};



#endif // GLOBAL_TRANSLATOR_HH
