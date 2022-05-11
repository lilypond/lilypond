/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music-function.hh"

LY_DEFINE (ly_music_function_extract, "ly:music-function-extract", 1, 0, 0,
           (SCM x),
           R"(
Return the Scheme function inside@tie{}@var{x}.
           )")
{
  auto *const fn = LY_ASSERT_SMOB (Music_function, x, 1);

  return fn->get_function ();
}

LY_DEFINE (ly_music_function_signature, "ly:music-function-signature", 1, 0, 0,
           (SCM x),
           R"(
Return the function signature inside@tie{}@var{x}.
           )")
{
  auto *const fn = LY_ASSERT_SMOB (Music_function, x, 1);

  return fn->get_signature ();
}

LY_DEFINE (ly_make_music_function, "ly:make-music-function", 2, 0, 0,
           (SCM signature, SCM func),
           R"(
Make a function to process music, to be used for the parser.  @var{func} is the
function, and @var{signature} describes its arguments.  @var{signature}'s cdr
is a list containing either @code{ly:music?} predicates or other type
predicates.  Its car is the syntax function to call.
           )")
{
  LY_ASSERT_TYPE (ly_is_list, signature, 1);
  LY_ASSERT_TYPE (ly_is_procedure, func, 2);
  int n = 0;
  for (SCM p = signature; scm_is_pair (p); p = scm_cdr (p), ++n)
    {
      SCM proc = scm_car (p);
      if (scm_is_pair (proc))
        proc = scm_car (proc);
      if (scm_is_false (scm_procedure_p (proc)))
        {
          scm_wrong_type_arg_msg ("music-function", n, p,
                                  "music function predicate");
        }
    }

  return Music_function::make_smob (signature, func);
}
