/*
  span-score-bar.cc -- implement Span_score_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "span-score-bar.hh"
#include "atom.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "main.hh"

Span_score_bar::Span_score_bar()
{
}

void
Score_bar::do_pre_processing ()
{
  type_str_ = "|";
  if (break_status_dir() != RIGHT) 
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

IMPLEMENT_IS_TYPE_B2(Span_score_bar, Span_bar, Score_bar);

  
