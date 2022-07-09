/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                2017 David Kastrup <dak@gnu.org>


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

#include "item.hh"
#include "laissez-vibrer-engraver.hh"

#include "translator.icc"

using std::vector;

class Repeat_tie_engraver : public Laissez_vibrer_engraver
{
  bool is_my_event_class (Stream_event *ev) override;
  Grob *make_my_tie (SCM cause) override;
  Grob *make_my_column (SCM cause) override;

public:
  TRANSLATOR_DECLARATIONS (Repeat_tie_engraver);
};

Repeat_tie_engraver::Repeat_tie_engraver (Context *c)
  : Laissez_vibrer_engraver (c)
{
}

bool
Repeat_tie_engraver::is_my_event_class (Stream_event *ev)
{
  return ev->in_event_class ("repeat-tie-event");
}

Grob *
Repeat_tie_engraver::make_my_tie (SCM cause)
{
  return make_item ("RepeatTie", cause);
}

Grob *
Repeat_tie_engraver::make_my_column (SCM cause)
{
  return make_item ("RepeatTieColumn", cause);
}

void
Repeat_tie_engraver::boot ()
{
  ADD_LISTENER_FOR (listen_laissez_vibrer, repeat_tie);
  ADD_ACKNOWLEDGER (note_head);
}

ADD_TRANSLATOR (Repeat_tie_engraver,
                /* doc */
                R"(
Create repeat ties.
                )",

                /* create */
                R"(
RepeatTie
RepeatTieColumn
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
