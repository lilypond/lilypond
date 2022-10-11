/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>


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
#include "paper-column.hh"

LY_DEFINE (ly_item_p, "ly:item?", 1, 0, 0, (SCM g),
           R"(
Is @var{g} an @code{Item} object?
           )")
{
  return to_scm (static_cast<bool> (unsmob<Item> (g)));
}

LY_DEFINE (ly_item_break_dir, "ly:item-break-dir", 1, 0, 0, (SCM it),
           R"(
The break status direction of item @var{it}.  @w{@code{-1}} means end of line,
@code{0}@tie{}unbroken, and @code{1}@tie{}beginning of line.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Item, it, 1);
  return to_scm (me->break_status_dir ());
}

LY_DEFINE (ly_item_get_column, "ly:item-get-column", 1, 0, 0, (SCM it),
           R"(
Return the @code{PaperColumn} or @code{NonMusicalPaperColumn} associated with
this @code{Item}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Item, it, 1);

  if (Paper_column *col = me->get_column ())
    return col->self_scm ();

  return SCM_EOL;
}
