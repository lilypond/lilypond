/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2026 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "music-wrapper-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "lily-imports.hh"
#include "music.hh"

// Iterator for `\measureRemainder music`, which is effectively
//
//     {
//       \initialContextFrom %{ copy of music %}
//       \setMeasureLengthFromHere %{ duration of music %}
//       music |
//       \setDefaultMeasureLength
//     }
//
// This is implemented in C++ so that it can compute the duration of the music
// dynamically.
class Measure_remainder_iterator final : public Music_wrapper_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Measure_remainder_iterator () = default;

protected:
  void process (Moment) override;
  void send_change_event (Direction d);
  void send_check_event ();

private:
  Context_handle event_handler_;
  bool started_ = false;
  bool stopped_ = false;
};

void
Measure_remainder_iterator::send_change_event (Direction d)
{
  if ((d == START) && !music_get_length ().main_part_)
    {
      // After we iterate the wrapped music, we will still be at the same main
      // moment, and then we will restore measureLength to the value specified
      // by the time signature, so there is no need to change it now.
      //
      // Skipping this event avoids warning at a measure boundary, where it
      // would naturally try to set measureLength = 0 (which is invalid).
      return;
    }

  SCM ev_scm = Lily::make_music (ly_symbol2scm ("MeasureLengthChangeEvent"));
  auto *ev = unsmob<Music> (ev_scm);

  auto *mus = get_music ();
  ev->set_spot (*mus->origin ());
  if (d == START)
    {
      const auto dur = Duration (music_get_length ().main_part_);
      set_property (ev, "duration", dur.smobbed_copy ());
    }

  ev->send_to_context (event_handler_.get ());
  scm_remember_upto_here_1 (ev_scm);
}

void
Measure_remainder_iterator::send_check_event ()
{
  SCM ev_scm = Lily::make_music (ly_symbol2scm ("BarCheckEvent"));
  auto *ev = unsmob<Music> (ev_scm);
  auto *mus = get_music ();
  ev->set_spot (*mus->origin ());
  ev->send_to_context (event_handler_.get ());
  scm_remember_upto_here_1 (ev_scm);
}

void
Measure_remainder_iterator::process (Moment m)
{
  if (!started_)
    {
      started_ = true;
      event_handler_ = get_context ();
      send_change_event (START);
    }

  Music_wrapper_iterator::process (m);

  if (started_ && !stopped_ && (m == music_get_length ()))
    {
      stopped_ = true;
      if (event_handler_)
        {
          send_check_event ();
          send_change_event (STOP);
          event_handler_ = nullptr;
        }
    }
}

IMPLEMENT_CTOR_CALLBACK (Measure_remainder_iterator);
