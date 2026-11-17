/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2026 Mike Solomon <mike@mikesolomon.org>


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

#include "lily-imports.hh"

#include <limits>

namespace
{
// State of the current pure evaluation.  Plain integers: nothing here is
// visible to (or needs protection from) the garbage collector.
struct Pure_state
{
  bool is_pure = false;
  vsize start = 0;
  vsize end = std::numeric_limits<vsize>::max ();
};

Pure_state pure_state;

void
restore_pure_state (void *saved)
{
  pure_state = *static_cast<Pure_state *> (saved);
}

// Adapts a legacy pure callback with the signature (grob start end . rest) to
// the uniform signature (grob . rest), by supplying the range of the current
// pure context for the two extra arguments.
class Legacy_pure_call : public Smob1<Legacy_pure_call>
{
public:
  LY_DECLARE_SMOB_PROC (&Legacy_pure_call::call, 1, 0, 1)
  SCM call (SCM grob, SCM rest)
  {
    return scm_apply_3 (scm1 (), grob, to_scm (Pure_context::start ()),
                        to_scm (Pure_context::end ()), rest);
  }
};

// The number of required arguments of PROC, or -1 if that cannot be
// determined (in which case callers assume the uniform signature).
int
required_args (SCM proc)
{
  if (!ly_is_procedure (proc))
    return -1;
  SCM arity = Guile_user::procedure_minimum_arity (proc);
  if (!scm_is_pair (arity) || !scm_is_integer (scm_car (arity)))
    return -1;
  return from_scm<int> (scm_car (arity));
}

SCM
apply_in_state (const Pure_state &state, SCM proc, SCM args)
{
  Pure_state saved = pure_state;
  // The unwind handler also runs when the frame is left via a non-local exit
  // (error, throw), where C++ destructors cannot be relied upon.
  scm_dynwind_begin (static_cast<scm_t_dynwind_flags> (0));
  scm_dynwind_unwind_handler (restore_pure_state, &saved,
                              SCM_F_WIND_EXPLICITLY);
  pure_state = state;
  SCM result = scm_apply_0 (proc, args);
  scm_dynwind_end ();
  return result;
}
} // namespace

SCM
Pure_context::apply (vsize start, vsize end, SCM proc, SCM args)
{
  // Nested lookups for the same range are the common case; avoid the cost of
  // setting up a dynwind frame for them.
  if (pure_state.is_pure && pure_state.start == start && pure_state.end == end)
    return scm_apply_0 (proc, args);

  return apply_in_state ({true, start, end}, proc, args);
}

SCM
Pure_context::apply_unpure (SCM proc, SCM args)
{
  // The common case, by far.
  if (!pure_state.is_pure)
    return scm_apply_0 (proc, args);

  return apply_in_state ({}, proc, args);
}

bool
Pure_context::is_pure ()
{
  return pure_state.is_pure;
}

vsize
Pure_context::start ()
{
  return pure_state.start;
}

vsize
Pure_context::end ()
{
  return pure_state.end;
}

SCM
Unpure_pure_container::pure_part () const
{
  return SCM_UNBNDP (scm2 ()) ? scm1 () : scm2 ();
}

SCM
Unpure_pure_container::make_smob (SCM unpure, SCM pure)
{
  if (SCM_UNBNDP (pure))
    {
      // A single callback is "unchanging", see is_unchanging ().
      if (!ly_is_procedure (unpure))
        pure = unpure;
    }
  else
    {
      // Legacy signature: the pure callback takes two more required arguments
      // (start and end) than the unpure one.  If there is no procedure to
      // compare to, the unpure part is taken to be a plain grob callback.
      int pure_req = required_args (pure);
      int unpure_req = ly_is_procedure (unpure) ? required_args (unpure) : 1;
      if (pure_req >= 0 && unpure_req >= 0 && pure_req >= unpure_req + 2)
        pure = Legacy_pure_call::make_smob (pure);
    }

  return Smob2<Unpure_pure_container>::make_smob (unpure, pure);
}

const char *const Unpure_pure_container::type_p_name_
  = "ly:unpure-pure-container?";

LY_DEFINE (ly_make_unpure_pure_container, "ly:make-unpure-pure-container", 1,
           1, 0, (SCM unpure, SCM pure),
           R"(
Make an unpure-pure container.  @var{unpure} should be an unpure expression,
and @var{pure} should be a pure expression.  If @var{pure} is omitted,
the value of @var{unpure} will be used for pure calculations as well.  A
callback is then evaluated like an unpure one (and its result is cached) even
for pure lookups, and cannot make use of @code{(*pure?*)}, @code{(*start*)}
and @code{(*end*)}.  A callback that needs to behave in a pure way has to be
given as both @var{unpure} and @var{pure}.

A pure callback is called with the same arguments as the unpure one.  If it
needs the range of break columns it is evaluated for, it can retrieve it with
@code{(*start*)} and @code{(*end*)}; @code{(*pure?*)} tells whether a pure
evaluation is going on at all.

For compatibility, a @var{pure} procedure taking two required arguments more
than @var{unpure} is assumed to be written in the legacy form
@code{(@var{grob} @var{start} @var{end} @dots{})} and is called accordingly.
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
  auto display_part = [&port] (SCM part) {
    // One common type are procedures. If it has a name, print that.
    if (ly_is_procedure (part))
      {
        SCM name = scm_procedure_name (part);
        if (ly_is_symbol (name))
          {
            scm_display (name, port);
            return;
          }
      }

    scm_display (part, port);
  };

  scm_puts ("#<unpure-pure-container ", port);
  display_part (unpure_part ());
  if (!is_unchanging ())
    {
      scm_puts (" ", port);
      display_part (pure_part ());
    }
  scm_puts (" >", port);
  return 1;
}

LY_DEFINE (ly_pure_call, "ly:pure-call", 4, 0, 1,
           (SCM data, SCM grob, SCM start, SCM end, SCM rest),
           R"(
Convert property @var{data} (unpure-pure container, procedure or plain value)
to value in a pure context defined by @var{grob}, @var{start}, @var{end}, and
possibly @var{rest} arguments, in the same way as it is done when a property is
looked up in a pure calculation.  The pure part of a container is called as
@code{(@var{grob} . @var{rest})}; @var{start} and @var{end} are available to it
as @code{(*start*)} and @code{(*end*)}.  A procedure that is not part of a
container is not evaluated in pure calculations and yields @code{#f}; use
@code{ly:pure-apply} to call a procedure in a pure context.
           )")
{
  if (Unpure_pure_container *upc = unsmob<Unpure_pure_container> (data))
    {
      data = upc->pure_part ();
      if (!ly_is_procedure (data))
        return data;
      // A single callback is evaluated like an unpure one.
      if (upc->is_unchanging ())
        return Pure_context::apply_unpure (data, scm_cons (grob, rest));
      return Pure_context::apply (from_scm<vsize> (start), from_scm<vsize> (end),
                                  data, scm_cons (grob, rest));
    }
  return ly_is_procedure (data) ? SCM_BOOL_F : data;
}

LY_DEFINE (ly_pure_apply, "ly:pure-apply", 4, 0, 1,
           (SCM proc, SCM grob, SCM start, SCM end, SCM rest),
           R"(
Call procedure @var{proc} as @code{(@var{proc} @var{grob} . @var{rest})} in a
pure context for the range of columns from @var{start} to @var{end}, which are
available to it as @code{(*start*)} and @code{(*end*)}.  This is for calling a
pure callback directly rather than through a property or an unpure-pure
container (@code{ly:pure-call} would not evaluate a plain procedure).
           )")
{
  LY_ASSERT_TYPE (ly_is_procedure, proc, 1);
  return Pure_context::apply (from_scm<vsize> (start), from_scm<vsize> (end),
                              proc, scm_cons (grob, rest));
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
    return Pure_context::apply_unpure (data, scm_cons (grob, rest));
  return data;
}

LY_DEFINE (ly_pure_start, "ly:pure-start", 0, 0, 0, (),
           R"(
Return the index of the first break column of the range for which a pure
property is currently being evaluated, or @code{#f} outside of a pure
evaluation.  The preferred way of calling this is @code{(*start*)}.
           )")
{
  return Pure_context::is_pure () ? to_scm (Pure_context::start ())
                                  : SCM_BOOL_F;
}

LY_DEFINE (ly_pure_end, "ly:pure-end", 0, 0, 0, (),
           R"(
Return the index of the last break column of the range for which a pure
property is currently being evaluated, or @code{#f} outside of a pure
evaluation.  The preferred way of calling this is @code{(*end*)}.
           )")
{
  return Pure_context::is_pure () ? to_scm (Pure_context::end ()) : SCM_BOOL_F;
}

LY_DEFINE (ly_pure_p, "ly:pure?", 0, 0, 0, (),
           R"(
Return @code{#t} if a pure property is currently being evaluated.  This is
for callbacks that behave differently in the pure and the final calculation
without actually needing the range of columns.  The preferred way of calling
this is @code{(*pure?*)}.
           )")
{
  return to_scm (Pure_context::is_pure ());
}
