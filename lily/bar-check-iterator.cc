/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "simple-music-iterator.hh"

/*
  Check bar checks. We do this outside the engravers so that you can
  race through the score using skipTypesetting to correct durations.
*/
class Bar_check_iterator final : Simple_music_iterator
{
public:
  void process (Moment) override;
  Bar_check_iterator ();
  DECLARE_SCHEME_CALLBACK (constructor, ());

private:
  void check ();
};

IMPLEMENT_CTOR_CALLBACK (Bar_check_iterator);

Bar_check_iterator::Bar_check_iterator ()
{
}

void
Bar_check_iterator::check ()
{
  auto *const tr = get_context ();

  if (from_scm<bool> (get_property (tr, "ignoreBarChecks")))
    return;

  SCM mp_scm = SCM_EOL;
  auto *const timing = where_defined (tr, "measurePosition", &mp_scm);
  if (!timing)
    return;

  const auto mp = from_scm (mp_scm, Moment ());
  if (!mp.main_part_) // at start of measure: check passed
    return;

  bool warn = true;
  if (from_scm<bool> (get_property (tr, "barCheckSynchronize")))
    {
      set_property (timing, "measurePosition", to_scm (Moment ()));
    }
  else
    {
      if (mp == from_scm (get_property (tr, "barCheckLastFail"), Moment ()))
        warn = false;
      else
        set_property (tr, "barCheckLastFail", mp_scm);
    }

  if (warn)
    warning (_f ("barcheck failed at: %s", to_string (mp)));
}

void
Bar_check_iterator::process (Moment m)
{
  if (!has_started ())
    check ();
  Simple_music_iterator::process (m);
}
