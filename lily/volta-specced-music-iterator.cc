/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "alternative-sequence-iterator.hh"
#include "context.hh"
#include "input.hh"
#include "lily-imports.hh"
#include "music.hh"

class Volta_specced_music_iterator final : public Music_wrapper_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Volta_specced_music_iterator () = default;

protected:
  SCM create_event (Direction d);
  void process (Moment) override;

private:
  Context_handle event_handler_;
  size_t volta_depth_ = 0;
  bool started_ = false;
  bool stopped_ = false;
};

SCM
Volta_specced_music_iterator::create_event (Direction d)
{
  SCM ev_scm
    = Lily::make_span_event (ly_symbol2scm ("VoltaSpanEvent"), to_scm (d));
  auto *ev = unsmob<Music> (ev_scm);

  auto *mus = get_music ();
  ev->set_spot (*mus->origin ());
  set_property (ev, "volta-depth", to_scm (volta_depth_));
  set_property (ev, "volta-numbers", get_property (mus, "volta-numbers"));
  // TODO: tweaks? (see Tuplet_iterator)

  // must return a SCM to ensure the object is kept alive.
  return ev_scm;
}

void
Volta_specced_music_iterator::process (Moment m)
{
  if (!started_)
    {
      // The result of Alternative_sequence_iterator::volta_brackets_enabled ()
      // is not accurate until process ().  If not for that, all this could
      // have been prepared in create_children ().

      if (auto *const parent
          = dynamic_cast<Alternative_sequence_iterator *> (get_parent ()))
        {
          volta_depth_ = parent->volta_bracket_depth ();

          // Let the Alternative_sequence_iterator veto the bracket, e.g. for
          // the tail alternatives of a \repeat segno.
          if (!parent->volta_brackets_enabled ())
            stopped_ = true;
        }
      else
        {
          // Do not create volta brackets except for children of \alternative.
          //
          // TODO: Consider sending a different event and developing an
          // engraver to engrave similar brackets at Staff (or Voice?) level,
          // more like ottava brackets than volta brackets.
          stopped_ = true;
        }

      started_ = stopped_;
    }

  // TODO: Test empty music and grace notes (probably won't work as-is).
  if (!started_)
    {
      started_ = true;
      event_handler_ = get_context ();
      SCM ev = create_event (START);
      unsmob<Music> (ev)->send_to_context (event_handler_.get ());
      scm_remember_upto_here_1 (ev);
    }

  Music_wrapper_iterator::process (m);

  if (started_ && !stopped_ && (m == music_get_length ()))
    {
      stopped_ = true;
      if (event_handler_)
        {
          SCM ev = create_event (STOP);
          unsmob<Music> (ev)->send_to_context (event_handler_.get ());
          scm_remember_upto_here_1 (ev);
          event_handler_ = nullptr;
        }
    }
}

IMPLEMENT_CTOR_CALLBACK (Volta_specced_music_iterator);
