/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "pointer-group-interface.hh"
#include "grob.hh"

LY_DEFINE (ly_pointer_group_interface__add_grob,
           "ly:pointer-group-interface::add-grob", 3, 0, 0,
           (SCM grob, SCM sym, SCM grob_element),
           R"(
Add @var{grob-element} to @var{grob}'s @var{sym} grob array.
           )")
{
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);
  auto *const elem = LY_ASSERT_SMOB (Grob, grob_element, 3);

  Pointer_group_interface::add_grob (g, sym, elem);
  return SCM_UNSPECIFIED;
}
