/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "input.hh"
#include "international.hh"
#include "music.hh"

using std::string;

class Relative_octave_check
{
public:
  DECLARE_SCHEME_CALLBACK (relative_callback, (SCM, SCM));
};

MAKE_SCHEME_CALLBACK (Relative_octave_check, relative_callback,
                      "ly:relative-octave-check::relative-callback", 2)
SCM
Relative_octave_check::relative_callback (SCM music, SCM last_pitch)
{
  Pitch p = *unsmob<Pitch> (last_pitch);
  auto *const m = LY_ASSERT_SMOB (Music, music, 1);
  Pitch *check_p = unsmob<Pitch> (get_property (m, "pitch"));

  int delta_oct = 0;
  if (check_p)
    {
      Pitch no_octave (-1, check_p->get_notename (),
                       check_p->get_alteration ());

      Pitch result = no_octave.to_relative_octave (p);

      if (result != *check_p)
        {
          string s = _ ("Failed octave check, got: ");
          s += result.to_string ();

          m->warning (s);

          delta_oct = check_p->get_octave () - result.get_octave ();
        }
    }

  return Pitch (p.get_octave () + delta_oct, p.get_notename (),
                p.get_alteration ())
    .smobbed_copy ();
}
