/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Joe Neeman <joeneeman@gmail.com>

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
#include "one-line-page-breaking.hh"
#include "one-line-auto-height-breaking.hh"
#include "one-page-breaking.hh"
#include "optimal-page-breaking.hh"
#include "minimal-page-breaking.hh"

LY_DEFINE (ly_page_turn_breaking, "ly:page-turn-breaking", 1, 0, 0,
           (SCM paper_book),
           R"(
Optimally break (pages and lines) the @code{Paper_book} object @var{paper-book}
such that page turns only happen in specified places, returning its pages.
           )")
{
  auto *const pb = LY_ASSERT_SMOB (Paper_book, paper_book, 1);
  Page_turn_page_breaking b (pb);
  return b.solve ();
}

LY_DEFINE (ly_optimal_breaking, "ly:optimal-breaking", 1, 0, 0,
           (SCM paper_book),
           R"(
Optimally break (pages and lines) the @code{Paper_book} object @var{paper-book}
to minimize badness for both vertical and horizontal spacing.
           )")
{
  auto *const pb = LY_ASSERT_SMOB (Paper_book, paper_book, 1);
  Optimal_page_breaking b (pb);
  return b.solve ();
}

LY_DEFINE (ly_minimal_breaking, "ly:minimal-breaking", 1, 0, 0,
           (SCM paper_book),
           R"(
Break (pages and lines) the @code{Paper_book} object @var{paper-book} without
looking for optimal spacing: stack as many lines on a page before moving to the
next one.
           )")
{
  auto *const pb = LY_ASSERT_SMOB (Paper_book, paper_book, 1);
  Minimal_page_breaking b (pb);
  return b.solve ();
}

LY_DEFINE (ly_one_page_breaking, "ly:one-page-breaking", 1, 0, 0,
           (SCM paper_book),
           R"(
Put each score on a single page.  The @code{paper-height} settings are modified
so each score fits on one page, and the height of the page matches the height
of the full score.
           )")
{
  auto *const pb = LY_ASSERT_SMOB (Paper_book, paper_book, 1);
  One_page_breaking b (pb);
  return b.solve ();
}

LY_DEFINE (ly_one_line_breaking, "ly:one-line-breaking", 1, 0, 0,
           (SCM paper_book),
           R"(
Put each score on a single line, and put each line on its own page.  Modify the
@code{paper-width} setting so that every page is wider than the widest line.
           )")
{
  auto *const pb = LY_ASSERT_SMOB (Paper_book, paper_book, 1);
  One_line_page_breaking b (pb);
  return b.solve ();
}

LY_DEFINE (ly_one_line_auto_height_breaking, "ly:one-line-auto-height-breaking",
           1, 0, 0, (SCM paper_book),
           R"(
Put each score on a single line, and put each line on its own page.  Modify the
@code{paper-width} setting so that every page is wider than the widest line.
Modify the @code{paper-height} setting to fit the height of the tallest line.
           )")
{
  auto *const pb = LY_ASSERT_SMOB (Paper_book, paper_book, 1);
  One_line_auto_height_breaking b (pb);
  return b.solve ();
}
