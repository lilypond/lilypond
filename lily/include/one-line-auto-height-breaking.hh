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

#ifndef ONE_LINE_AUTO_HEIGHT_BREAKING_HH
#define ONE_LINE_AUTO_HEIGHT_BREAKING_HH

#include "one-line-page-breaking.hh"
#include "page-breaking.hh"

class One_line_auto_height_breaking : public One_line_page_breaking
{
public:
  SCM solve () override;

  One_line_auto_height_breaking (Paper_book *pb);
  virtual ~One_line_auto_height_breaking ();
};

#endif /* ONE_LINE_AUTO_HEIGHT_BREAKING_HH */
