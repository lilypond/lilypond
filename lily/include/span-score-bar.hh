/*
  span-score-bar.hh -- declare Span_score_bar

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_SCORE_BAR_HH
#define SPAN_SCORE_BAR_HH

#include "span-bar.hh"
#include "score-bar.hh"

class Span_score_bar : public Span_bar, public Score_bar
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEM_CLONE(Span_score_bar);
  Span_score_bar();
protected:
  virtual void do_pre_processing();
};


class Piano_brace : public Span_score_bar
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEM_CLONE(Piano_brace);
protected:
  virtual Interval do_width() const;
  virtual Atom get_bar_sym (Real) const;
};

class Staff_bracket : public Span_score_bar
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEM_CLONE(Staff_bracket);
protected:
  virtual Interval do_width() const;
  virtual Atom get_bar_sym (Real) const;
};

#endif // SPAN_SCORE_BAR_HH
