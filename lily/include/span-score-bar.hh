/*
  span-score-bar.hh -- declare Span_score_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_SCORE_BAR_HH
#define SPAN_SCORE_BAR_HH

#include "span-bar.hh"
#include "score-bar.hh"

class Span_score_bar : public Span_bar, public Score_bar
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEMENT_CLONE(Span_score_bar);
  Span_score_bar();

protected:
  virtual void do_pre_processing();
};



#endif // SPAN_SCORE_BAR_HH
