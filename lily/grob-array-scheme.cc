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

#include "grob-array.hh"
#include "grob.hh"

LY_DEFINE (ly_grob_array_length, "ly:grob-array-length",
	   1, 0, 0,
	   (SCM grob_arr),
	   "Return the length of @var{grob-arr}.")
{
  LY_ASSERT_SMOB (Grob_array, grob_arr, 1);

  Grob_array *me = unsmob_grob_array (grob_arr);
  return  scm_from_int (me->size ());
}


LY_DEFINE (ly_grob_array_ref, "ly:grob-array-ref",
	   2, 0, 0,
	   (SCM grob_arr, SCM index),
	   "Retrieve the @var{index}th element of @var{grob-arr}.")
{
  Grob_array *me = unsmob_grob_array (grob_arr);
  LY_ASSERT_SMOB (Grob_array, grob_arr, 1);
  LY_ASSERT_TYPE (scm_is_integer, index, 2);

  vsize i = scm_to_unsigned (index);
  if (i == VPOS || i >= me->size ())
    scm_out_of_range (NULL, scm_from_unsigned (i)); 
  
  return me->grob (i)->self_scm ();
}

