/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Neil Puttock <n.puttock@gmail.com>

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

#include "simple-music-iterator.hh"

#include "context.hh"
#include "global-context.hh"
#include "input.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "moment.hh"
#include "music.hh"

class Partial_iterator final : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_SCHEME_CALLBACK (finalization, (SCM, SCM));

protected:
  void process (Moment) override;
};

void
Partial_iterator::process (Moment m)
{
  if (Duration *dur
      = unsmob<Duration> (get_property (get_music (), "duration")))
    {
      Moment length = Moment (dur->get_length ());

      SCM mp_scm = SCM_EOL;
      auto *timing = where_defined (get_context (), "measurePosition", &mp_scm);

      if (!timing)
        programming_error ("missing Timing in \\partial");
      else if (get_context ()->now_mom () > 0)
        {
          set_property (timing, "partialBusy", SCM_BOOL_T);
          Global_context *g = find_global_context (get_context ());
          g->add_finalization (ly_list (finalization_proc, timing->self_scm (),
                                        length.smobbed_copy ()));
        }
      else
        {
          auto mp = from_scm (mp_scm, Moment (0));
          mp.main_part_ = 0;
          set_property (timing, "measurePosition",
                        (mp - length).smobbed_copy ());
        }
    }
  else
    programming_error ("invalid duration in \\partial");

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Partial_iterator);

MAKE_SCHEME_CALLBACK (Partial_iterator, finalization,
                      "ly:partial-iterator::finalization", 2);
SCM
Partial_iterator::finalization (SCM timing_scm, SCM length_scm)
{
  auto *const timing = LY_ASSERT_SMOB (Context, timing_scm, 1);
  auto *const length = LY_ASSERT_SMOB (Moment, length_scm, 2);

  auto mp = from_scm (get_property (timing, "measurePosition"), Moment (0));
  mp.main_part_ = measure_length (timing);
  set_property (timing, "measurePosition", (mp - *length).smobbed_copy ());
  timing->unset_property (ly_symbol2scm ("partialBusy"));

  return SCM_UNSPECIFIED;
}
