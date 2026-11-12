/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef UNPURE_PURE_CONTAINER_HH
#define UNPURE_PURE_CONTAINER_HH

#include "lily-guile.hh"
#include "small-smobs.hh"

// The dynamic context of a "pure" evaluation: the range of break columns
// [start, end] for which an estimate is requested.  Pure callbacks have the
// same signature as unpure ones and, if they need the range, retrieve it
// from here (Scheme: `(*start*)', `(*end*)', `(*pure?*)').
//
// All the machinery is confined to this class.  The state consists of plain
// integers (nothing for the garbage collector to scan) and is saved and
// restored with a Guile dynwind frame, so it is also restored on non-local
// exits.  Nested calls with an unchanged range do not set up a new frame.
class Pure_context
{
public:
  // Apply `proc' to the list `args' in a pure context for [start, end].
  static SCM apply (vsize start, vsize end, SCM proc, SCM args);
  // Apply `proc' to the list `args' outside of a pure context, even if a pure
  // evaluation is going on.  This is for unpure callbacks, which must not see
  // the context of a pure calculation that happens to trigger them.
  static SCM apply_unpure (SCM proc, SCM args);

  // Are we currently inside a pure evaluation?
  static bool is_pure ();
  // Only meaningful if is_pure ().  Outside of a pure evaluation, the
  // range is reported as unbounded (the whole score).
  static vsize start ();
  static vsize end ();
};

class Unpure_pure_container : public Smob2<Unpure_pure_container>
{
public:
  static const char *const type_p_name_;
  SCM unpure_part () const { return scm1 (); }
  // A container made from a single callback.  The callback is evaluated like
  // an unpure one (and cached) for pure lookups, too.  It thus must not make
  // use of the pure context (the range of break columns), and gives the same
  // answer for tentative and final layouts.  A callback that is meant to
  // behave in a pure way has to be given as both parts.
  bool is_unchanging () const { return SCM_UNBNDP (scm2 ()); }
  SCM pure_part () const;
  // If `pure' is a procedure with the legacy signature (grob start end ...),
  // i.e., taking two required arguments more than `unpure', it is wrapped
  // into a procedure with the signature of `unpure'.
  static SCM make_smob (SCM unpure, SCM pure = SCM_UNDEFINED);
  int print_smob (SCM, scm_print_state *) const;
};

#endif /* UNPURE_PURE_CONTAINER_HH */
