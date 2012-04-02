/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012 Mike Solomon <mike@apollinemike.com>

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

#include "rhythmic-music-iterator.hh"

#include "context.hh"
#include "dispatcher.hh"
#include "duration.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "warn.hh"

Rhythmic_music_iterator::Rhythmic_music_iterator ()
{
}

void
Rhythmic_music_iterator::construct_children ()
{
  Simple_music_iterator::construct_children ();
  descend_to_bottom_context ();
}

void
Rhythmic_music_iterator::process (Moment m)
{
  if (last_processed_mom_ < Moment (0))
    {

      descend_to_bottom_context ();

      Context *c = get_outlet ();
      Stream_event *ev = get_music ()->to_event ();
      SCM arts = ev->get_property ("articulations");

      if (scm_is_pair (arts))
        {
          // There is no point in broadcasting articulations like
          // harmonic events that nobody listens to.  Those work
          // exclusively as articulations.
          SCM listened = SCM_EOL;
          SCM unlistened = SCM_EOL;
          for (; scm_is_pair (arts); arts = scm_cdr (arts))
            {
              if (scm_is_true
                  (scm_call_2
                   (ly_lily_module_constant ("any"),
                    ly_lily_module_constant ("ly:is-listened-event-class"),
                    scm_call_1
                    (ly_lily_module_constant ("ly:make-event-class"),
                     unsmob_stream_event (scm_car (arts))
                     ->get_property ("class")))))
                listened = scm_cons (scm_car (arts), listened);
              else
                unlistened = scm_cons (scm_car (arts), unlistened);
            }
          ev->set_property ("articulations", scm_reverse_x (unlistened, SCM_EOL));
          c->event_source ()->broadcast (ev);
          arts = scm_reverse_x (listened, SCM_EOL);
          for (; scm_is_pair (arts); arts = scm_cdr (arts))
            c->event_source ()->broadcast (unsmob_stream_event (scm_car (arts)));
        }
      else
        c->event_source ()->broadcast (ev);

      ev->unprotect ();
    }
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Rhythmic_music_iterator);
