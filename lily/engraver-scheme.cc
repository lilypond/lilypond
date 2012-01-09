/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

LY_DEFINE (ly_engraver_make_grob, "ly:engraver-make-grob",
           3, 0, 0, (SCM engraver, SCM grob_name, SCM cause),
           "Create a grob originating from given @var{engraver} instance,"
           " with given @var{grob-name}, a symbol."
           "  @var{cause} should either be another grob"
           " or a music event.")
{
  LY_ASSERT_TYPE (unsmob_engraver, engraver, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob_name, 2);
  LY_ASSERT_TYPE (ly_is_grob_cause, cause, 3);

  Grob *g = unsmob_engraver (engraver)->
            internal_make_grob (grob_name, cause,
                                ly_symbol2string (grob_name).c_str (),
                                "scheme", 0, "scheme");
  return g->self_scm ();
}

LY_DEFINE (ly_engraver_announce_end_grob, "ly:engraver-announce-end-grob",
           3, 0, 0, (SCM engraver, SCM grob, SCM cause),
           "Announce the end of a grob (i.e., the end of a spanner)"
           " originating from given @var{engraver} instance, with"
           " @var{grob} being a grob.  @var{cause} should either"
           " be another grob or a music event.")
{
  LY_ASSERT_TYPE (unsmob_engraver, engraver, 1);
  LY_ASSERT_SMOB (Grob, grob, 2);
  LY_ASSERT_TYPE (ly_is_grob_cause, cause, 3);

  unsmob_engraver (engraver)->
  announce_end_grob (unsmob_grob (grob), cause);

  return SCM_UNSPECIFIED;
}
