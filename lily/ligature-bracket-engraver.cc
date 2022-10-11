/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Juergen Reuter <reuter@ipd.uka.de>

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

#include "international.hh"

#include "engraver.hh"
#include "note-column.hh"
#include "tuplet-bracket.hh"
#include "span-event-listener.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "item.hh"

#include "translator.icc"

class Ligature_bracket_engraver : public Engraver
{
protected:
  void process_music ();
  void stop_translation_timestep ();
  void acknowledge_rest (Grob_info);
  void acknowledge_note_column (Grob_info);
  void listen_ligature (Stream_event *);

public:
  TRANSLATOR_DECLARATIONS (Ligature_bracket_engraver);

private:
  Unique_span_event_listener ligature_listener_;
  Spanner *ligature_ = nullptr;
};

Ligature_bracket_engraver::Ligature_bracket_engraver (Context *c)
  : Engraver (c)
{
}

void
Ligature_bracket_engraver::process_music ()
{
  if (auto *const ender = ligature_listener_.get_stop ())
    {
      if (!ligature_)
        {
          ender->warning (_ ("cannot find start of ligature"));
          return;
        }

      ligature_ = nullptr;
    }

  if (auto *const starter = ligature_listener_.get_start ())
    {
      if (ligature_)
        {
          starter->warning (_ ("already have a ligature"));
          ligature_->warning (_ ("ligature was started here"));
          return;
        }

      ligature_ = make_spanner ("LigatureBracket", starter->self_scm ());
    }
}

void
Ligature_bracket_engraver::acknowledge_note_column (Grob_info info)
{
  if (ligature_)
    {
      // TODO: We might see a MultiMeasureRest here, which is a Spanner, when
      // called from acknowledge_rest ().  What then?  Is passing a null
      // pointer to these functions OK?
      Item *item = dynamic_cast<Item *> (info.grob ());
      Tuplet_bracket::add_column (ligature_, item);
      add_bound_item (ligature_, item);
    }
}

void
Ligature_bracket_engraver::acknowledge_rest (Grob_info info)
{
  acknowledge_note_column (info);
}

void
Ligature_bracket_engraver::stop_translation_timestep ()
{
  ligature_listener_.reset ();
}

void
Ligature_bracket_engraver::boot ()
{
  ADD_DELEGATE_LISTENER (ligature);
  ADD_ACKNOWLEDGER (rest);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Ligature_bracket_engraver,
                /* doc */
                R"(
Handle @code{Ligature_events} by engraving @code{Ligature} brackets.
                )",

                /* create */
                R"(
LigatureBracket
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
