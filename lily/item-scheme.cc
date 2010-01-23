/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "item.hh"


LY_DEFINE (ly_item_p, "ly:item?",
	   1, 0, 0, (SCM g),
	   "Is @var{g} an @code{Item} object?")
{
  Grob *me = unsmob_grob (g);
  bool b = dynamic_cast<Item *> (me);
  return ly_bool2scm (b);
}

LY_DEFINE (ly_item_break_dir, "ly:item-break-dir",
	   1, 0, 0, (SCM it),
	   "The break status direction of item @var{it}.  @code{-1} means"
	   " end of line, @code{0}@tie{}unbroken, and"
	   " @code{1}@tie{}beginning of line.")
{
  LY_ASSERT_TYPE (unsmob_item, it, 1);
  Item *me = unsmob_item (it);
  return scm_from_int (me->break_status_dir ());
}
