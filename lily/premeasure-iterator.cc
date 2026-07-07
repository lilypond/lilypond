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

#include "input.hh"
#include "lily-imports.hh"
#include "music.hh"

// Iterator for `\premeasure music`, which is effectively
//
//     {
//       \initialContextFrom %{ copy of music %}
//       \partial %{ duration of music %}
//       music |
//     }
//
// This is implemented as an iterator so that it can compute the duration of
// the music after things like \removeWithTag have had a chance to change it.
class Premeasure_iterator final : public Music_wrapper_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Premeasure_iterator () = default;

protected:
  void process (Moment) override;
  void send_partial_event ();
  void send_check_event ();

private:
  bool started_ = false;
  bool stopped_ = false;
};

void
Premeasure_iterator::send_partial_event ()
{
  SCM ev_scm = Lily::make_music (ly_symbol2scm ("PartialEvent"));
  auto *ev = unsmob<Music> (ev_scm);
  auto *mus = get_music ();
  ev->set_spot (*mus->origin ());
  const auto dur = Duration (music_get_length ().main_part_);
  set_property (ev, "duration", dur.smobbed_copy ());
  ev->send_to_context (get_context ());
  scm_remember_upto_here_1 (ev_scm);
}

void
Premeasure_iterator::send_check_event ()
{
  SCM ev_scm = Lily::make_music (ly_symbol2scm ("BarCheckEvent"));
  auto *ev = unsmob<Music> (ev_scm);
  auto *mus = get_music ();
  ev->set_spot (*mus->origin ());
  ev->send_to_context (get_context ());
  scm_remember_upto_here_1 (ev_scm);
}

void
Premeasure_iterator::process (Moment m)
{
  if (!started_)
    {
      started_ = true;
      send_partial_event ();
    }

  Music_wrapper_iterator::process (m);

  if (started_ && !stopped_ && (m == music_get_length ()))
    {
      stopped_ = true;
      send_check_event ();
    }
}

IMPLEMENT_CTOR_CALLBACK (Premeasure_iterator);
