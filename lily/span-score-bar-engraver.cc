/*
  span-score-bar-engraver.cc -- implement Span_score_bar_engraver,
  Piano_bar_engraver and Staff_group_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "span-bar.hh"
#include "span-score-bar-engraver.hh"
#include "paper-def.hh"
#include "dimension-cache.hh"

Span_bar*
Span_score_bar_engraver::get_span_bar_p () const
{
  Span_bar*s =  new Span_bar;
  s->type_str_ = "scorebar";
  s->set_elt_property ("break-aligned",SCM_BOOL_T);

  return s;
}


Span_score_bar_engraver::Span_score_bar_engraver ()
{
  use_priority_b_ = true;
}

Span_bar*
Piano_bar_engraver::get_span_bar_p () const
{
  Span_bar *s= new Span_bar;
  s->set_empty (X_AXIS);
  s->type_str_ = "brace";
  return s;
}

Span_bar*
Staff_group_bar_engraver::get_span_bar_p () const
{
  Span_bar *s= new Span_bar;
  s->set_empty (X_AXIS);
  s->type_str_ = "bracket";
  return s;
}

void
Staff_group_bar_engraver::acknowledge_element (Score_element_info i)
{
  Base_span_bar_engraver::acknowledge_element (i);
  if (Span_bar * b = dynamic_cast<Span_bar *> (i.elem_l_))
    {
      if (b->type_str_ == "brace")
	b->translate_axis ( -paper_l ()->get_var ("interline"),
			    X_AXIS); // ugh
    }
}

ADD_THIS_TRANSLATOR (Piano_bar_engraver);
ADD_THIS_TRANSLATOR (Staff_group_bar_engraver);
ADD_THIS_TRANSLATOR (Span_score_bar_engraver);


