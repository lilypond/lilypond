/*
  span-score-bar-grav.hh -- declare Span_score_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_SCORE_BAR_GRAV_HH
#define SPAN_SCORE_BAR_GRAV_HH

#include "span-bar-grav.hh"

class Span_score_bar_engraver : public Span_bar_engraver 
{
public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    virtual Span_bar* get_span_bar_p()const;
};

/**
  Please don't shoot the piano player
 */
class Piano_bar_engraver :  public Span_score_bar_engraver
{
public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    virtual Span_bar * get_span_bar_p() const;
};

#endif // SPAN_SCORE_BAR_GRAV_HH
