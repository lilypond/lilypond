/*
  global-context.hh -- declare Global_context

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef GLOBAL_CONTEXT_HH
#define GLOBAL_CONTEXT_HH

#include "context.hh"
#include "pqueue.hh"

class Global_context : public virtual Context
{
  PQueue<Moment> extra_mom_pq_;
  Output_def *output_def_;

  DECLARE_CLASSNAME(Global_context);

  friend class Output_def;
public:
  Global_context (Output_def *, Moment final, Object_key *key);
  int get_moments_left () const;
  Moment sneaky_insert_extra_moment (Moment);
  void add_moment_to_process (Moment);
  void run_iterator_on_me (Music_iterator *);
  virtual Score_context *get_score_context () const;

  void apply_finalizations ();
  void add_finalization (SCM);

  virtual SCM get_output ();
  virtual void prepare (Moment);
  virtual void one_time_step ();
  virtual void finish ();
  virtual Output_def *get_output_def () const;
  virtual Moment now_mom () const;
  virtual Context *get_default_interpreter ();

  Moment previous_moment () const;
protected:
  Moment final_mom_;
  Moment prev_mom_;
  Moment now_mom_;
};

SCM ly_format_output (SCM);

#endif // GLOBAL_CONTEXT_HH
