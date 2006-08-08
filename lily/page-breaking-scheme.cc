/*
  page-breaking-scheme.cc -- implement bindings to the various
  page-breakers

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#include "paper-book.hh"
#include "page-turn-page-breaking.hh"
#include "optimal-page-breaking.hh"

LY_DEFINE (ly_page_turn_breaking, "ly:page-turn-breaking",
	   1, 0, 0, (SCM pb),
	   "Optimally break (pages and lines) the Paper_book PB such that page turns "
	   "only happen in specified places, returning its pages.")
{
  Page_turn_page_breaking b (unsmob_paper_book (pb));
  return b.solve ();
}

LY_DEFINE (ly_optimal_breaking, "ly:optimal-breaking",
	   1, 0, 0, (SCM pb),
	   "Optimally break (pages and lines) the Paper_book PB to minimise badness in "
	   "bother vertical and horizontal spacing.")
{
  Optimal_page_breaking b (unsmob_paper_book (pb));
  return b.solve ();
}
