
/*
  span-score-bar.cc -- implement Span_score_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "piano-brace.hh"
#include "atom.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "main.hh"

Piano_brace::Piano_brace ()
{
  extra_move_left_f_ = 0.0;
}

Atom
Piano_brace::get_bar_sym (Real dy) const
{
  Atom a = lookup_l ()->vbrace (dy);
  a.translate_axis (-extra_move_left_f_, X_AXIS);

      
  return a;
}

Interval
Piano_brace::do_width() const
{
  return Interval (0,0);
}

void
Piano_brace::do_post_processing ()
{
  Span_score_bar::do_post_processing();
  Interval i = Span_score_bar::do_height ();
  Real staffheight_f = paper ()->staffheight_f ();

  // don't set braces that span only one staff
  if (i.length () <= 2.0 * staffheight_f)
    {
      set_empty (true);
      transparent_b_ = true;
    }
}



  
