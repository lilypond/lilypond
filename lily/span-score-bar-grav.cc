/*
  span-score-bar-grav.cc -- implement Span_score_bar_engraver and Piano_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "span-score-bar.hh"
#include "span-score-bar-grav.hh"


Span_bar*
Span_score_bar_engraver::get_span_bar_p() const
{
    return new Span_score_bar;
}


IMPLEMENT_IS_TYPE_B1(Span_score_bar_engraver, Span_bar_engraver);
IMPLEMENT_IS_TYPE_B1(Piano_bar_engraver, Span_score_bar_engraver);

Span_bar*
Piano_bar_engraver::get_span_bar_p() const
{
    return new Piano_brace;
}

ADD_THIS_ENGRAVER(Piano_bar_engraver);
ADD_THIS_ENGRAVER(Span_score_bar_engraver);
