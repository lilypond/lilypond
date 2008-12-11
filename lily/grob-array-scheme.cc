/*
  grob-array-scheme.cc -- implement Grob_array bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

