/*
  global-translator.hh -- declare Global_translator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef GLOBAL_TRANSLATOR_HH
#define GLOBAL_TRANSLATOR_HH

#include "translator-group.hh"
#include "rational.hh"
#include "pqueue.hh"

class Global_translator : public virtual Translator_group{
  PQueue<Moment> extra_mom_pq_;
public:
  TRANSLATOR_CLONE(Global_translator);
  Moment last_mom_;
  Moment now_mom_;
  Global_translator();

  int moments_left_i() const;
  void modify_next (Moment&);
  void add_moment_to_process (Moment);

  virtual Music_output *get_output_p ();     
  virtual void prepare (Moment);
  virtual void process() {}
  virtual void finish() {}
  virtual void start() {}

  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual Moment now_moment () const;
  virtual Global_translator *global_l() { return this; }
};



#endif // GLOBAL_TRANSLATOR_HH
