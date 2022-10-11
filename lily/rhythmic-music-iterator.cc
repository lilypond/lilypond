/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2022 Mike Solomon <mike@mikesolomon.org>

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
Rhythmic_music_iterator::create_contexts ()
{
  Simple_music_iterator::create_contexts ();
  descend_to_bottom_context ();
}

void
Rhythmic_music_iterator::process (Moment m)
{
  if (!has_started ())
    {
      descend_to_bottom_context ();

      Context *c = get_context ();
      Stream_event *ev = get_music ()->to_event ();
      SCM arts = get_property (ev, "articulations");

      if (scm_is_pair (arts))
        {
          // There is no point in broadcasting articulations like
          // harmonic events that nobody listens to.  Those work
          // exclusively as articulations.
          SCM listened = SCM_EOL;
          SCM unlistened = SCM_EOL;
          for (; scm_is_pair (arts); arts = scm_cdr (arts))
            {
              SCM art = scm_car (arts);

              if (c->event_source ()->is_listened_class (
                    get_property (unsmob<Stream_event> (art), "class")))
                listened = scm_cons (art, listened);
              else
                unlistened = scm_cons (art, unlistened);
            }
          set_property (ev, "articulations",
                        scm_reverse_x (unlistened, SCM_EOL));
          c->event_source ()->broadcast (ev);
          arts = scm_reverse_x (listened, SCM_EOL);
          for (; scm_is_pair (arts); arts = scm_cdr (arts))
            c->event_source ()->broadcast (
              unsmob<Stream_event> (scm_car (arts)));
        }
      else
        c->event_source ()->broadcast (ev);

      ev->unprotect ();
    }
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Rhythmic_music_iterator);
