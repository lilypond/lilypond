/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver.hh"
#include "grob.hh"
#include "item.hh"
#include "spanner.hh"

LY_DEFINE (ly_engraver_make_grob, "ly:engraver-make-grob", 3, 0, 0,
           (SCM engraver, SCM grob_name, SCM cause),
           R"(
Create a grob originating from given @var{engraver} instance, with given
@var{grob-name}, a symbol.  @var{cause} should either be another grob or a
music event.
           )")
{
  auto *const en = LY_ASSERT_SMOB (Engraver, engraver, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob_name, 2);
  LY_ASSERT_TYPE (ly_is_grob_cause, cause, 3);

  return en
    ->internal_make_indeterminate (grob_name, cause, "scheme", 0, "scheme")
    ->self_scm ();
}

LY_DEFINE (ly_engraver_make_item, "ly:engraver-make-item", 3, 0, 0,
           (SCM engraver, SCM grob_name, SCM cause),
           R"(
Same as @code{ly:engraver-make-grob}, but always create a grob with the
@code{Item} class.  This is useful when the same grob definition is used to
create grobs of differing classes.
           )")
{
  auto *const en = LY_ASSERT_SMOB (Engraver, engraver, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob_name, 2);
  LY_ASSERT_TYPE (ly_is_grob_cause, cause, 3);

  return en->internal_make_item (grob_name, cause, "scheme", 0, "scheme")
    ->self_scm ();
}

LY_DEFINE (ly_engraver_make_spanner, "ly:engraver-make-spanner", 3, 0, 0,
           (SCM engraver, SCM grob_name, SCM cause),
           R"(
Same as @code{ly:engraver-make-grob}, but always create a grob with the
@code{Spanner} class.  This is useful when the same grob definition is used to
create grobs of differing classes.
           )")
{
  auto *const en = LY_ASSERT_SMOB (Engraver, engraver, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob_name, 2);
  LY_ASSERT_TYPE (ly_is_grob_cause, cause, 3);

  return en->internal_make_spanner (grob_name, cause, "scheme", 0, "scheme")
    ->self_scm ();
}

LY_DEFINE (ly_engraver_make_sticky, "ly:engraver-make-sticky", 4, 0, 0,
           (SCM engraver, SCM grob_name, SCM host, SCM cause),
           R"(
Utility function to create a grob sticking to another grob.  This acts like
either @code{ly:@/engraver-make-item} or @code{ly:@/engraver-make-spanner},
depending on the class of the host.  Additionally, the host is made the parent
of the newly created sticky grob on the y@tie{}axis and, for items, on the
x@tie{}axis.  Sticky spanners take their bounds from their host and their end
is announced with the end of the host.

 Sticky grobs must have the @code{sticky-grob-interface} interface, see
@rinternals{sticky-grob-interface}.
           )")
{
  auto *const en = LY_ASSERT_SMOB (Engraver, engraver, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob_name, 2);
  auto *const h = LY_ASSERT_SMOB (Grob, host, 3);
  LY_ASSERT_TYPE (ly_is_grob_cause, cause, 4);
  Grob *g
    = en->internal_make_sticky (grob_name, h, cause, "scheme", 0, "scheme");
  return g->self_scm ();
}

LY_DEFINE (ly_engraver_announce_end_grob, "ly:engraver-announce-end-grob", 3, 0,
           0, (SCM engraver, SCM grob, SCM cause),
           R"(
Announce the end of a grob (i.e., the end of a spanner) originating from given
@var{engraver} instance, with @var{grob} being a grob.  @var{cause} should
either be another grob or a music event.
           )")
{
  auto *const en = LY_ASSERT_SMOB (Engraver, engraver, 1);
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 2);
  LY_ASSERT_TYPE (ly_is_grob_cause, cause, 3);

  if (!en->context ()->get_parent ())
    programming_error ("context for engraver has been detached");
  else
    en->announce_end_grob (g, cause);

  return SCM_UNSPECIFIED;
}
