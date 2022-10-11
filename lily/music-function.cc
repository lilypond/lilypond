/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "lily-parser.hh"
#include "input.hh"
#include "music.hh"
#include "fluid.hh"
#include "lily-imports.hh"

const char *const Music_function::type_p_name_ = "ly:music-function?";

/* Print a textual represenation of the smob to a given port.  */
int
Music_function::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Music function ", port);
  scm_write (get_function (), port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}

// Used for attaching location information to music expressions in
// default arguments and return values.  Music expressions taken from
// the call signature need to be cloned since they are not suitable
// for multiple use.

static SCM
with_loc (SCM arg, Fluid &loc, bool clone = true)
{
  if (Music *m = unsmob<Music> (arg))
    {
      if (clone)
        {
          m = m->clone ();
          arg = m->unprotect ();
        }
      if (Input *in = unsmob<Input> (loc))
        m->set_spot (*in);
    }
  return arg;
}

// A music function call implies walking through the call signature
// and matching the actual argument list to the signature.  This
// process is not 1:1 due to the possible presence of optional
// arguments which are handled quite differently from how GUILE/Scheme
// usually deal with optional arguments.
//
// The argument matching here intentionally closely tracks the
// semantics of calls via the LilyPond parser as described in
// <URL:lilypond.org/doc/Documentation/extending/scheme-function-usage>:
// if an optional argument predicate does not match the next argument
// from the actual argument list, the default given in the call
// signature is used instead and all following optional arguments are
// unconditionally substituted in a similar manner.
//
// This skipping of optional arguments can be explicitly initiated by
// using \default in LilyPond.  The respective value to use for a call
// via Scheme is *unspecified*.

SCM
Music_function::call (SCM rest)
{
  Fluid location (Lily::f_location);

  // (car (ly:music-signature self_scm())) is the return type, skip it
  SCM signature = scm_cdr (get_signature ());

  // The main loop just processes the signature in sequence, and the
  // resulting actual arguments are accumulated in reverse order in args

  SCM args = SCM_EOL;

  while (scm_is_pair (rest) && scm_is_pair (signature))
    {
      SCM arg = scm_car (rest);
      SCM pred = scm_car (signature);
      if (!scm_is_pair (pred))
        {
          // non-optional argument
          if (scm_is_false (ly_call (pred, arg)))
            {
              Syntax::argument_error (scm_oneplus (scm_length (args)), pred,
                                      arg);
              SCM val = scm_car (get_signature ());
              val = scm_is_pair (val) ? scm_cdr (val) : SCM_BOOL_F;
              return with_loc (val, location);
            }
        }
      // If the predicate is not a function but a pair, it
      // signifies an optional argument.  This is not quite the
      // form declared to define-music-function (which is always
      // a proper list) but a pair of predicate function and
      // default value.
      //
      // Fall through to default argument processing when optional
      // argument predicate matches
      else if (scm_is_false (ly_call (scm_car (pred), arg)))
        {
          // optional argument, non-match
          // *unspecified* is the same as an explicit \default: skip it
          if (scm_is_eq (arg, SCM_UNSPECIFIED))
            rest = scm_cdr (rest);
          // Replace this and all following optional arguments with
          // their defaults:
          do
            {
              args = scm_cons (with_loc (scm_cdr (pred), location), args);
              signature = scm_cdr (signature);
              if (!scm_is_pair (signature))
                break;
              pred = scm_car (signature);
            }
          while (scm_is_pair (pred));
          continue;
        }
      // Normal processing of accepted argument
      signature = scm_cdr (signature);
      args = scm_cons (arg, args);
      rest = scm_cdr (rest);
    }

  // There may be trailing optional arguments: for those we don't
  // require an additional *unspecified* as a substitute for \default
  // since the end of the argument list is explicitly recognisable,
  // unlike with LilyPond syntactic entry.  That allows using things
  // like (key) in Scheme to serve the function of \key \default in LilyPond.

  for (; scm_is_pair (signature); signature = scm_cdr (signature))
    {
      SCM pred = scm_car (signature);
      if (!scm_is_pair (pred))
        break;

      args = scm_cons (with_loc (scm_cdr (pred), location), args);
    }

  if (scm_is_pair (rest) || scm_is_pair (signature))
    scm_wrong_num_args (self_scm ());

  SCM res = scm_apply_0 (get_function (), scm_reverse_x (args, SCM_EOL));

  SCM pred = scm_car (get_signature ());
  // The return type predicate may have the form of a pair in which
  // the car is the actual predicate and the cdr is the surrogate
  // return value in the error case, to be extracted by
  // music-function-call-error.
  if (scm_is_pair (pred))
    pred = scm_car (pred);

  if (scm_is_true (ly_call (pred, res)))
    return with_loc (res, location, false);

  return Syntax::music_function_call_error (self_scm (), res);
}
