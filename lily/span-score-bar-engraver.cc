/*
  span-score-bar-engraver.cc -- implement Span_score_bar_engraver,
  Piano_bar_engraver and Staff_group_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "span-bar.hh"
#include "span-score-bar-engraver.hh"
#include "paper-def.hh"

Span_bar*
Span_score_bar_engraver::get_span_bar_p () const
{
  Span_bar*s =  new Span_bar;
  s->type_str_ = "scorebar";
    
  return s;
}


Span_score_bar_engraver::Span_score_bar_engraver ()
{
  use_priority_b_ = true;
  break_priority_i_ = -4;
}

Span_bar*
Piano_bar_engraver::get_span_bar_p () const
{
  Span_bar *s= new Span_bar;
  s->dim_cache_[X_AXIS].set_empty (true);
  s->type_str_ = "{";
  return s;
}

Span_bar*
Staff_group_bar_engraver::get_span_bar_p () const
{
  Span_bar *s= new Span_bar;
  s->dim_cache_[X_AXIS].set_empty (true);
  s->type_str_ = "[";
  return s;
}

void
Staff_group_bar_engraver::acknowledge_element (Score_element_info i)
{
  Base_span_bar_engraver::acknowledge_element (i);
  if (Span_bar * b = dynamic_cast<Span_bar *> (i.elem_l_))
    {
      if (b->type_str_ == "{")
	b->extra_x_off_ -=  paper_l ()->get_realvar (interline_scm_sym); // ugh
    }
}

ADD_THIS_TRANSLATOR (Piano_bar_engraver);
ADD_THIS_TRANSLATOR (Staff_group_bar_engraver);
ADD_THIS_TRANSLATOR (Span_score_bar_engraver);

