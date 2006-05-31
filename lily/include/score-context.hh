/*
  score-context.hh -- declare Score_context

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#ifndef SCORE_CONTEXT_HH
#define SCORE_CONTEXT_HH

#include "global-context.hh"

class Score_context : public Context
{
public:
  Score_context (Object_key const *);

  virtual SCM get_output ();
  virtual void prepare (Moment);
  virtual void finish ();
  virtual void one_time_step ();
};

#endif /* SCORE_CONTEXT_HH */
