/*
  span-score-bar-engraver.hh -- declare Span_score_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_SCORE_BAR_GRAV_HH
#define SPAN_SCORE_BAR_GRAV_HH

#include "base-span-bar-engraver.hh"

/** 

  Make the bars that Span the entire score line (system). A
  Span_bar_engraver which generates a special bar.

  */

class Span_score_bar_engraver : public Base_span_bar_engraver 
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Span_score_bar_engraver ();
  virtual Span_bar* get_span_bar_p () const;
};

/**
  Make a piano brace.  (No, this doesn't manufacture ``Please don't
  shoot the piano player.''  signs)
  */
class Piano_bar_engraver :  public Span_score_bar_engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
  virtual Span_bar * get_span_bar_p () const;
};

/**
  Make Choir brackets.
 */
class Staff_group_bar_engraver :  public Span_score_bar_engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
  virtual Span_bar * get_span_bar_p () const;
  virtual void acknowledge_element (Score_element_info);
};

#endif // SPAN_SCORE_BAR_GRAV_HH
