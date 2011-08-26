/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2011 Han-Wen Nienhuys <hanwen@lilypond.org>


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

LY_DEFINE (ly_spanner_bound, "ly:spanner-bound",
           2, 0, 0, (SCM spanner, SCM dir),
           "Get one of the bounds of @var{spanner}.  @var{dir} is @w{@code{-1}}"
           " for left, and @code{1} for right.")
{
  LY_ASSERT_TYPE (unsmob_spanner, spanner, 1);
  LY_ASSERT_TYPE (is_direction, dir, 2);
  Item *bound = unsmob_spanner (spanner)->get_bound (to_dir (dir));
  return bound ? bound->self_scm () : SCM_EOL;
}

LY_DEFINE (ly_spanner_set_bound_x, "ly:spanner-set-bound!",
           3, 0, 0, (SCM spanner, SCM dir, SCM item),
           "Set grob @var{item} as bound in direction @var{dir} for"
           " @var{spanner}.")
{
  LY_ASSERT_TYPE (unsmob_spanner, spanner, 1);
  LY_ASSERT_TYPE (is_direction, dir, 2);
  LY_ASSERT_TYPE (unsmob_item, item, 3);

  unsmob_spanner (spanner)->set_bound (to_dir (dir), unsmob_item (item));
  return SCM_UNSPECIFIED;
}

/* TODO: maybe we should return a vector -- random access is more
   logical for this list? */
LY_DEFINE (ly_spanner_broken_into, "ly:spanner-broken-into",
           1, 0, 0, (SCM spanner),
           "Return broken-into list for @var{spanner}.")
{
  LY_ASSERT_TYPE (unsmob_spanner, spanner, 1);
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (spanner));

  SCM s = SCM_EOL;
  for (vsize i = me->broken_intos_.size (); i--;)
    s = scm_cons (me->broken_intos_[i]->self_scm (), s);
  return s;
}

LY_DEFINE (ly_spanner_p, "ly:spanner?",
           1, 0, 0, (SCM g),
           "Is @var{g} a spanner object?")
{
  Grob *me = unsmob_grob (g);
  bool b = dynamic_cast<Spanner *> (me);

  return ly_bool2scm (b);
}
