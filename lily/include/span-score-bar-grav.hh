/*
  span-score-bar-grav.hh -- declare Span_score_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_SCORE_BAR_GRAV_HH
#define SPAN_SCORE_BAR_GRAV_HH

#include "span-bar-grav.hh"

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
  Please don't shoot the piano player
 */
class Piano_bar_engraver :  public Span_score_bar_engraver
{
public:
  TRANSLATOR_CLONE (Piano_bar_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Span_bar * get_span_bar_p () const;
};

/**
  
 */
class Staff_group_bar_engraver :  public Span_score_bar_engraver
{
public:
  TRANSLATOR_CLONE (Staff_group_bar_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Span_bar * get_span_bar_p () const;
  virtual void acknowledge_element (Score_elem_info);
};

#endif // SPAN_SCORE_BAR_GRAV_HH
