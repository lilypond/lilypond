/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "spanner.hh"
#include "item.hh"

LY_DEFINE (ly_spanner_bound, "ly:spanner-bound", 2, 1, 0,
           (SCM spanner, SCM dir, SCM def),
           R"(
Get one of the bounds of @var{spanner}.  @var{dir} is @w{@code{-1}} for left,
and @code{1} for right.  If the spanner does not (yet) have a bound
for this direction, return @var{def}, or @code{'()} if @var{def}
is not specified.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Spanner, spanner, 1);
  LY_ASSERT_TYPE (is_scm<Direction>, dir, 2);
  Item *bound = me->get_bound (from_scm<Direction> (dir));
  if (bound)
    return bound->self_scm ();
  if (SCM_UNBNDP (def))
    return SCM_EOL;
  return def;
}

LY_DEFINE (ly_spanner_set_bound_x, "ly:spanner-set-bound!", 3, 0, 0,
           (SCM spanner, SCM dir, SCM item),
           R"(
Set grob @var{item} as bound in direction @var{dir} for @var{spanner}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Spanner, spanner, 1);
  LY_ASSERT_TYPE (is_scm<Direction>, dir, 2);
  auto *const it = LY_ASSERT_SMOB (Item, item, 3);

  me->set_bound (from_scm<Direction> (dir), it);
  return SCM_UNSPECIFIED;
}

/* TODO: maybe we should return a vector -- random access is more
   logical for this list? */
LY_DEFINE (ly_spanner_broken_into, "ly:spanner-broken-into", 1, 0, 0,
           (SCM spanner),
           R"(
Return broken-into list for @var{spanner}.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Spanner, spanner, 1);

  SCM s = SCM_EOL;
  for (vsize i = me->broken_intos_.size (); i--;)
    s = scm_cons (me->broken_intos_[i]->self_scm (), s);
  return s;
}

LY_DEFINE (ly_spanner_p, "ly:spanner?", 1, 0, 0, (SCM g),
           R"(
Is @var{g} a spanner object?
           )")
{
  return to_scm (static_cast<bool> (unsmob<Spanner> (g)));
}
