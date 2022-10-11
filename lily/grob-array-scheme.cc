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

#include "grob-array.hh"
#include "grob.hh"

LY_DEFINE (ly_grob_array_length, "ly:grob-array-length", 1, 0, 0,
           (SCM grob_arr),
           R"(
Return the length of @var{grob-arr}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob_array, grob_arr, 1);

  return to_scm (me->size ());
}

LY_DEFINE (ly_grob_array_ref, "ly:grob-array-ref", 2, 0, 0,
           (SCM grob_arr, SCM index),
           R"(
Retrieve the @var{index}th element of @var{grob-arr}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob_array, grob_arr, 1);
  LY_ASSERT_TYPE (scm_is_integer, index, 2);

  vsize i = scm_to_uint (index);
  if (i == VPOS || i >= me->size ())
    scm_out_of_range (NULL, to_scm (i));

  return me->grob (i)->self_scm ();
}

LY_DEFINE (ly_grob_array_2_list, "ly:grob-array->list", 1, 0, 0, (SCM grob_arr),
           R"(
Return the elements of @var{grob-arr} as a Scheme list.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob_array, grob_arr, 1);

  return grob_array_to_list (me);
}

LY_DEFINE (ly_grob_list_2_grob_array, "ly:grob-list->grob-array", 1, 0, 0,
           (SCM grob_list),
           R"(
Convert a Scheme list of grobs to a grob array.
           )")
{
  return grob_list_to_grob_array (grob_list);
}
