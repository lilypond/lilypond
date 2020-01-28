/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2016--2020 Paul Morris

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

#ifndef ONE_PAGE_BREAKING_HH
#define ONE_PAGE_BREAKING_HH

#include "page-breaking.hh"
#include "page-spacing.hh"

class One_page_breaking : public Page_breaking
{
public:
  SCM solve () override;

  One_page_breaking (Paper_book *pb);
  virtual ~One_page_breaking ();

private:
  SCM read_spacing_alist (SCM spec, SCM sym);
};

#endif /* ONE_PAGE_BREAKING_HH */
