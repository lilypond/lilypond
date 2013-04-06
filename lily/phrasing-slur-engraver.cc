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

#include "context.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "note-column.hh"
#include "slur.hh"
#include "slur-proto-engraver.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Phrasing_slur_engraver : public Slur_proto_engraver
{
protected:
  DECLARE_TRANSLATOR_LISTENER (phrasing_slur);
  DECLARE_TRANSLATOR_LISTENER (break_phrasing_slur);
  DECLARE_ACKNOWLEDGER (slur);

public:
  TRANSLATOR_DECLARATIONS (Phrasing_slur_engraver);
};

Phrasing_slur_engraver::Phrasing_slur_engraver () :
  Slur_proto_engraver (0, "PhrasingSlur", "phrasing slur", "phrasing-slur-event")
{
  break_slur_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Phrasing_slur_engraver, phrasing_slur);
void
Phrasing_slur_engraver::listen_phrasing_slur (Stream_event *ev)
{
  internal_listen_slur (ev);
}

IMPLEMENT_TRANSLATOR_LISTENER (Phrasing_slur_engraver, break_phrasing_slur);
void
Phrasing_slur_engraver::listen_break_phrasing_slur (Stream_event *ev)
{
  internal_listen_break_slur (ev);
}

void
Phrasing_slur_engraver::acknowledge_slur (Grob_info info)
{
  acknowledge_extra_object (info);
}

ADD_ACKNOWLEDGER (Phrasing_slur_engraver, inline_accidental);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, fingering)
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, note_column);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, slur);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, script);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, dots);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, text_script);
ADD_END_ACKNOWLEDGER (Phrasing_slur_engraver, tie);
ADD_ACKNOWLEDGER (Phrasing_slur_engraver, tuplet_number);

ADD_TRANSLATOR (Phrasing_slur_engraver,
                /* doc */
                "Print phrasing slurs.  Similar to @ref{Slur_engraver}.",

                /* create */
                "PhrasingSlur ",

                /* read */
                "",

                /* write */
                ""
               );
