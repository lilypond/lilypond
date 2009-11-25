/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2009 Joe Neeman <joeneeman@gmail.com>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "paper-book.hh"
#include "page-turn-page-breaking.hh"
#include "optimal-page-breaking.hh"
#include "minimal-page-breaking.hh"

LY_DEFINE (ly_page_turn_breaking, "ly:page-turn-breaking",
	   1, 0, 0, (SCM pb),
	   "Optimally break (pages and lines) the @code{Paper_book} object"
	   " @var{pb} such that page turns only happen in specified places,"
	   " returning its pages.")
{
  Page_turn_page_breaking b (unsmob_paper_book (pb));
  return b.solve ();
}

LY_DEFINE (ly_optimal_breaking, "ly:optimal-breaking",
	   1, 0, 0, (SCM pb),
	   "Optimally break (pages and lines) the @code{Paper_book} object"
	   " @var{pb} to minimize badness in bother vertical and horizontal"
	   " spacing.")
{
  Optimal_page_breaking b (unsmob_paper_book (pb));
  return b.solve ();
}

LY_DEFINE (ly_minimal_breaking, "ly:minimal-breaking",
	   1, 0, 0, (SCM pb),
	   "Break (pages and lines) the @code{Paper_book} object @var{pb}"
	   " without looking for optimal spacing: stack as many lines on"
	   " a page before moving to the next one.")
{
  Minimal_page_breaking b (unsmob_paper_book (pb));
  return b.solve ();
}
