/*
  span-score-bar.cc -- implement Span_score_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "span-score-bar.hh"
#include "atom.hh"
#include "paper-def.hh"
#include "lookup.hh"

Span_score_bar::Span_score_bar()
{
}

void
Score_bar::do_pre_processing ()
{
  type_str_ = "|";
  if (break_status_i() != 1) 
    {
      set_empty (true);
      transparent_b_ = true;
    }
}

void
Span_score_bar::do_pre_processing()
{
  /*
    duh.  The order of these two is subtle. 
   */
  Score_bar::do_pre_processing ();
  //  Span_bar::do_pre_processing();
}

Atom
Piano_brace::get_bar_sym (Real dy) const
{
  return paper()->lookup_l ()->vbrace (dy);
}

Interval
Piano_brace::do_width() const
{
  return Interval (0,0);
}

Atom
Staff_bracket::get_bar_sym (Real dy) const
{
  Atom a = paper()->lookup_l ()->vbracket (dy);
  a.translate_axis (- 1.5 * a.extent ().x ().length (), X_AXIS);
  return a;
}

Interval
Staff_bracket::do_width() const
{
  return Interval (0,0);
}


IMPLEMENT_IS_TYPE_B2(Span_score_bar, Span_bar, Score_bar);
IMPLEMENT_IS_TYPE_B1(Piano_brace, Span_score_bar);
IMPLEMENT_IS_TYPE_B1(Staff_bracket, Span_score_bar);

  
