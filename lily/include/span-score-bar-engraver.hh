/*
  span-score-bar-engraver.hh -- declare Span_score_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_SCORE_BAR_GRAV_HH
#define SPAN_SCORE_BAR_GRAV_HH

#include "span-bar-engraver.hh"

/** 

  Make the bars that Span the entire score line (system). A
  Span_bar_engraver which generates a special bar.

  */

class Span_score_bar_engraver : public Span_bar_engraver 
{
public:
  TRANSLATOR_CLONE (Span_score_bar_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Span_bar* get_span_bar_p () const;
};

/**
  Make a piano brace.  (No, this doesn't manufacture ``Please don't
  shoot the piano player.''  signs)
  */
class Piano_bar_engraver :  public Span_score_bar_engraver
{
public:
  TRANSLATOR_CLONE (Piano_bar_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Span_bar * get_span_bar_p () const;
};

/**
  Make Choir brackets.
 */
class Staff_group_bar_engraver :  public Span_score_bar_engraver
{
public:
  TRANSLATOR_CLONE (Staff_group_bar_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Span_bar * get_span_bar_p () const;
  virtual void acknowledge_element (Score_element_info);
};

#endif // SPAN_SCORE_BAR_GRAV_HH
