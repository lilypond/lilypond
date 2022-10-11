/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "audio-item.hh"
#include "context.hh"
#include "mark-engraver.hh"
#include "performer.hh"
#include "stream-event.hh"
#include "translator.icc"

class Mark_performer final : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Mark_performer);

private:
  void process_music ();
};

Mark_performer::Mark_performer (Context *c)
  : Performer (c)
{
}

void
Mark_performer::process_music ()
{
  auto process_mark = [this] (auto get_text, SCM property_sym) {
    SCM text = get_text (context ());
    if (!scm_is_null (text))
      {
        // We could change the Mark_engraver's getter to give us this event
        // too, since it has to look it up internally.  It's not a big deal.
        SCM ev_scm = get_property (context (), property_sym);
        auto *const ev = unsmob<Stream_event> (ev_scm);
        announce<Audio_text> (ev, Audio_text::MARKER, text);
      }
  };

  process_mark (Mark_engraver::get_current_rehearsal_mark_text,
                ly_symbol2scm ("currentRehearsalMarkEvent"));

  process_mark (Mark_engraver::get_current_performance_mark_text,
                ly_symbol2scm ("currentPerformanceMarkEvent"));
}

void
Mark_performer::boot ()
{
}

ADD_TRANSLATOR (Mark_performer,
                /* doc */
                R"(
This performer emits MIDI markers for rehearsal marks, segno and coda marks,
and section labels.  The MIDI markers are derived from markup that is generated
as in the @code{Mark_@/engraver}.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
currentPerformanceMarkEvent
currentRehearsalMarkEvent
                )",

                /* write */
                R"(

                )");
