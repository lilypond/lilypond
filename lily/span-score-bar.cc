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
Span_score_bar::do_pre_processing()
{
  /*
    duh.  The order of these two is subtle. 
   */
  Score_bar::do_pre_processing ();
  //  Span_bar::do_pre_processing();
}



  
