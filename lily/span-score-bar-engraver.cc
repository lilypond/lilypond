/*
  span-score-bar-grav.cc -- implement Span_score_bar_engraver,
  Piano_bar_engraver and Staff_group_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "span-score-bar.hh"
#include "piano-brace.hh"
#include "staff-bracket.hh"
#include "span-score-bar-engraver.hh"
#include "paper-def.hh"


Span_bar*
Span_score_bar_engraver::get_span_bar_p () const
{
  Span_bar*s =  new Span_score_bar;
  s->break_priority_i_ = -4;
    
  return s;
}


IMPLEMENT_IS_TYPE_B1 (Span_score_bar_engraver, Span_bar_engraver);
IMPLEMENT_IS_TYPE_B1 (Piano_bar_engraver, Span_score_bar_engraver);
IMPLEMENT_IS_TYPE_B1 (Staff_group_bar_engraver, Span_score_bar_engraver);

Span_bar*
Piano_bar_engraver::get_span_bar_p () const
{
  Span_bar *s= new Piano_brace;
  s->break_priority_i_ = -4;
  return s;
}

Span_bar*
Staff_group_bar_engraver::get_span_bar_p () const
{
  Span_bar *s= new Staff_bracket;
  s->break_priority_i_ = -4;
  return s;
}

void
Staff_group_bar_engraver::acknowledge_element (Score_element_info i)
{
  Span_bar_engraver::acknowledge_element (i);
  if (i.elem_l_->is_type_b (Piano_brace::static_name ()))
    {
      Span_bar* b =  dynamic_cast <Span_bar *> (i.elem_l_);
      Piano_brace * piano_l = (Piano_brace*) b;
      piano_l->extra_move_left_f_  = paper ()->interline_f (); // ugh
    }
}

ADD_THIS_TRANSLATOR (Piano_bar_engraver);
ADD_THIS_TRANSLATOR (Staff_group_bar_engraver);
ADD_THIS_TRANSLATOR (Span_score_bar_engraver);

