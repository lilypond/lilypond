/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2022 Mike Solomon <mike@mikesolomon.org>


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
#include "unpure-pure-container.hh"

// Reroutes a call to the contained function after dropping second and
// third argument.  Used for applying an "unpure" function in a "pure"
// context.
class Unpure_pure_call : public Smob1<Unpure_pure_call>
{
public:
  // Smob procedures unfortunately can only take at most 3 SCM
  // arguments.  Otherwise we could use a "3, 0, 1" call signature and
  // not require an argument count check of our own.
  LY_DECLARE_SMOB_PROC (&Unpure_pure_call::call, 2, 0, 1)
  SCM call (SCM arg1, SCM, SCM rest)
  {
    if (!scm_is_pair (rest))
      scm_wrong_num_args (scm1 ());
    return scm_apply_1 (scm1 (), arg1, scm_cdr (rest));
  }
};

SCM
Unpure_pure_container::pure_part () const
{
  return SCM_UNBNDP (scm2 ()) ? Unpure_pure_call::make_smob (scm1 ()) : scm2 ();
}

const char *const Unpure_pure_container::type_p_name_
  = "ly:unpure-pure-container?";

LY_DEFINE (ly_make_unpure_pure_container, "ly:make-unpure-pure-container", 1, 1,
           0, (SCM unpure, SCM pure),
           R"(
Make an unpure-pure container.  @var{unpure} should be an unpure expression,
and @var{pure} should be a pure expression.  If @var{pure} is omitted, the
value of @var{unpure} will be used twice, except that a callback is given two
extra arguments that are ignored for the sake of pure calculations.
           )")
{
  return Unpure_pure_container::make_smob (unpure, pure);
}

LY_DEFINE (ly_unpure_pure_container_unpure_part,
           "ly:unpure-pure-container-unpure-part", 1, 0, 0, (SCM pc),
           R"(
Return the unpure part of @var{pc}.
           )")
{
  auto *const upc = LY_ASSERT_SMOB (Unpure_pure_container, pc, 1);
  return upc->unpure_part ();
}

LY_DEFINE (ly_unpure_pure_container_pure_part,
           "ly:unpure-pure-container-pure-part", 1, 0, 0, (SCM pc),
           R"(
Return the pure part of @var{pc}.
           )")
{
  auto *const upc = LY_ASSERT_SMOB (Unpure_pure_container, pc, 1);
  return upc->pure_part ();
}

int
Unpure_pure_container::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<unpure-pure-container ", port);
  scm_display (unpure_part (), port);
  if (!is_unchanging ())
    {
      scm_puts (" ", port);
      scm_display (pure_part (), port);
    }
  scm_puts (" >", port);
  return 1;
}

LY_DEFINE (ly_pure_call, "ly:pure-call", 4, 0, 1,
           (SCM data, SCM grob, SCM start, SCM end, SCM rest),
           R"(
Convert property @var{data} (unpure-pure container or procedure) to value in a
pure context defined by @var{grob}, @var{start}, @var{end}, and possibly
@var{rest} arguments.
           )")
{
  if (Unpure_pure_container *upc = unsmob<Unpure_pure_container> (data))
    {
      // Avoid gratuitous creation of an Unpure_pure_call
      if (upc->is_unchanging ())
        data = upc->unpure_part ();
      else
        {
          data = upc->pure_part ();
          if (ly_is_procedure (data))
            return scm_apply_3 (data, grob, start, end, rest);
          return data;
        }
    }
  if (ly_is_procedure (data))
    return scm_apply_1 (data, grob, rest);
  return data;
}

LY_DEFINE (ly_unpure_call, "ly:unpure-call", 2, 0, 1,
           (SCM data, SCM grob, SCM rest),
           R"(
Convert property @var{data} (unpure-pure container or procedure) to value in an
unpure context defined by @var{grob} and possibly @var{rest} arguments.
           )")
{
  if (Unpure_pure_container *upc = unsmob<Unpure_pure_container> (data))
    data = upc->unpure_part ();
  if (ly_is_procedure (data))
    return scm_apply_1 (data, grob, rest);
  return data;
}
