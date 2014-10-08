/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2014 Mike Solomon <mike@mikesolomon.org>


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

#include "grob.hh"

// Reroutes a call to the contained function after dropping last two
// arguments.  Used for applying an "unpure" function in a "pure"
// context.
class Unpure_pure_call : public Smob1<Unpure_pure_call>
{
public:
  LY_DECLARE_SMOB_PROC (2, 0, 1, (SCM self, SCM arg1, SCM arg2, SCM rest))
  {
    return scm_apply_0 (Unpure_pure_call::unsmob (self)->scm1 (),
                        scm_call_2 (ly_lily_module_constant ("drop-right"),
                                    scm_cons2 (arg1, arg2, rest),
                                    scm_from_int (2)));
  }
};

SCM
Unpure_pure_container::pure_part () const
{
  return SCM_UNBNDP (scm2 ())
    ? Unpure_pure_call::make_smob (scm1 ())
    : scm2 ();
}

const char Unpure_pure_container::type_p_name_[] = "ly:unpure-pure-container?";

LY_DEFINE (ly_make_unpure_pure_container, "ly:make-unpure-pure-container",
           1, 1, 0, (SCM unpure, SCM pure),
           "Make an unpure-pure container.  @var{unpure} should be an unpure"
           " expression, and @var{pure} should be a pure expression.  If @var{pure}"
           " is omitted, the value of @var{unpure} will be used twice,"
           " except that a callback is given two extra arguments"
           " that are ignored for the sake of pure calculations.")
{
  return Unpure_pure_container::make_smob (unpure, pure);
}

LY_DEFINE (ly_unpure_pure_container_unpure_part, "ly:unpure-pure-container-unpure-part",
           1, 0, 0, (SCM pc),
           "Return the unpure part of @var{pc}.")
{
  LY_ASSERT_SMOB (Unpure_pure_container, pc, 1);
  return Unpure_pure_container::unsmob (pc)->unpure_part ();
}

LY_DEFINE (ly_unpure_pure_container_pure_part, "ly:unpure-pure-container-pure-part",
           1, 0, 0, (SCM pc),
           "Return the pure part of @var{pc}.")
{
  LY_ASSERT_SMOB (Unpure_pure_container, pc, 1);
  return Unpure_pure_container::unsmob (pc)->pure_part ();
}

int
Unpure_pure_container::print_smob (SCM port, scm_print_state *)
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
