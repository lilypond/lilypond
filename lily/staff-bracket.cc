/*
  span-score-bar.cc -- implement Span_score_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-bracket.hh"
#include "atom.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "main.hh"

Atom
Staff_bracket::get_bar_sym (Real dy) const
{
  Atom a = lookup_l ()->vbracket (dy);

  a.translate_axis (- 1.33 * a.extent ().x ().length (), X_AXIS);
  return a;
}

Interval
Staff_bracket::do_width() const
{
  return Interval (0,0);
}


void
Staff_bracket::do_post_processing ()
{
  Span_score_bar::do_post_processing();
  Interval i = Span_score_bar::do_height ();
  // don't set bracket that spans less than one staff
  Real staffheight_f = paper ()->staffheight_f ();
  if (i.length () < 0.5 * staffheight_f)
    {
      transparent_b_ = true;
      set_empty (true);
    }
}

IMPLEMENT_IS_TYPE_B1(Staff_bracket, Span_score_bar);

  
