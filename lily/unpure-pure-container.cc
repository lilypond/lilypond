/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2012 Mike Solomon <mike@mikesolomon.org>


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

static scm_t_bits unpure_pure_container_tag;
static scm_t_bits unpure_pure_call_tag;
// Used for rerouting a function of (grob start end) to one of
// (grob)

bool
is_unpure_pure_container (SCM s)
{
  return (SCM_NIMP (s) && SCM_CELL_TYPE (s) == unpure_pure_container_tag);
}

SCM
unpure_pure_container_unpure_part (SCM smob)
{
  LY_ASSERT_TYPE (is_unpure_pure_container, smob, 1);
  return SCM_SMOB_OBJECT (smob);
}

SCM
unpure_pure_container_pure_part (SCM smob)
{
  LY_ASSERT_TYPE (is_unpure_pure_container, smob, 1);
  SCM res = SCM_SMOB_OBJECT_2 (smob);

  if (!SCM_UNBNDP (res))
    return res;

  SCM_NEWSMOB (res, unpure_pure_call_tag,
               SCM_UNPACK (unpure_pure_container_unpure_part (smob)));
  return res;
}

LY_DEFINE (ly_unpure_pure_container_p, "ly:unpure-pure-container?",
           1, 0, 0, (SCM clos),
           "Is @var{clos} an unpure pure container?")
{
  return scm_from_bool (is_unpure_pure_container (clos));
}

LY_DEFINE (ly_make_unpure_pure_container, "ly:make-unpure-pure-container",
           1, 1, 0, (SCM unpure, SCM pure),
           "Make an unpure-pure container.  @var{unpure} should be an unpure"
           " expression, and @var{pure} should be a pure expression.  If @var{pure}"
           " is omitted, the value of @var{unpure} will be used twice,"
           " except that a callback is given two extra arguments"
           " that are ignored for the sake of pure calculations.")
{
  SCM z;

  if (SCM_UNBNDP (pure) && !ly_is_procedure (unpure))
    pure = unpure;

  SCM_NEWSMOB2 (z, unpure_pure_container_tag, SCM_UNPACK (unpure), SCM_UNPACK (pure));
  return z;
}

LY_DEFINE (ly_unpure_pure_container_unpure_part, "ly:unpure-pure-container-unpure-part",
           1, 0, 0, (SCM pc),
           "Return the unpure part of @var{pc}.")
{
  LY_ASSERT_TYPE (is_unpure_pure_container, pc, 1);
  return unpure_pure_container_unpure_part (pc);
}

LY_DEFINE (ly_unpure_pure_container_pure_part, "ly:unpure-pure-container-pure-part",
           1, 0, 0, (SCM pc),
           "Return the pure part of @var{pc}.")
{
  LY_ASSERT_TYPE (is_unpure_pure_container, pc, 1);
  return unpure_pure_container_pure_part (pc);
}

int
print_unpure_pure_container (SCM s, SCM port, scm_print_state *)
{
  scm_puts ("#<unpure-pure-container ", port);
  scm_display (unpure_pure_container_unpure_part (s), port);
  if (!SCM_UNBNDP (SCM_SMOB_OBJECT_2 (s)))
    {
      scm_puts (" ", port);
      scm_display (unpure_pure_container_pure_part (s), port);
    }
  scm_puts (" >", port);
  return 1;
}

SCM
pure_mark (SCM smob)
{
  scm_gc_mark (SCM_SMOB_OBJECT (smob));
  return SCM_SMOB_OBJECT_2 (smob);
}

// Function signature has two fixed arguments so that dropping two
// will always work: if we have fewer to start with, it will trigger
// wrong-number-of-args in a sensible location rather than making
// drop-right barf.

SCM
apply_unpure_pure (SCM clo, SCM arg1, SCM arg2, SCM rest)
{  
  return scm_apply_0 (SCM_SMOB_OBJECT (clo),
                      scm_call_2 (ly_lily_module_constant ("drop-right"),
                                  scm_cons2 (arg1, arg2, rest),
                                  scm_from_int (2)));
}
  

void init_unpure_pure_container ()
{
  unpure_pure_container_tag = scm_make_smob_type ("unpure-pure-container", 0);
  scm_set_smob_mark (unpure_pure_container_tag, pure_mark);
  scm_set_smob_print (unpure_pure_container_tag, print_unpure_pure_container);
  unpure_pure_call_tag = scm_make_smob_type ("unpure-pure-call", 0);
  scm_set_smob_mark (unpure_pure_call_tag, scm_markcdr);
  scm_set_smob_apply (unpure_pure_call_tag,
                      (SCM (*)()) apply_unpure_pure, 2, 0, 1);
};

ADD_SCM_INIT_FUNC (unpure_pure_container, init_unpure_pure_container);
