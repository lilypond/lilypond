/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2022 Joe Neeman <joeneeman@gmail.com>

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

#include "one-line-auto-height-breaking.hh"

#include "one-line-page-breaking.hh"
#include "output-def.hh"
#include "paper-book.hh"

One_line_auto_height_breaking::One_line_auto_height_breaking (Paper_book *pb)
  : One_line_page_breaking (pb)
{
}

One_line_auto_height_breaking::~One_line_auto_height_breaking ()
{
}

/*
  This is a somewhat unconventional page-breaking algorithm.  Like
  ly:one-line-breaking, every score is put on a single page, whose width
  is enough to fit the entire score on one line.  Line breaks and page
  breaks are ignored, and the paper-width setting in the paper
  block is modified to fit the music.  Unlike ly:one-line-breaking
  the paper-height setting in the paper block is also modified to fit
  the music.
*/

SCM
One_line_auto_height_breaking::solve ()
{
  Real max_height = 0;
  SCM pages = solve_and_provide_max_height (max_height);

  // Alter paper-height so that it fits the height of the tallest system.
  Real top_margin
    = from_scm<double> (book_->paper ()->c_variable ("top-margin"), 0.0);
  Real bottom_margin
    = from_scm<double> (book_->paper ()->c_variable ("bottom-margin"), 0.0);
  Real height = max_height + top_margin + bottom_margin;
  book_->paper ()->set_variable (ly_symbol2scm ("paper-height"),
                                 to_scm (height));

  return pages;
}
