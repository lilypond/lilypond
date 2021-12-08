/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2021 Neil Puttock <n.puttock@gmail.com>

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
#include "international.hh"
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"

#include "translator.icc"

using std::vector;

class Episema_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Episema_engraver);
protected:
  void finalize () override;
  void listen_episema (Stream_event *);
  void acknowledge_note_column (Grob_info_t<Item>);
  void acknowledge_note_head (Grob_info);
  void stop_translation_timestep ();
  void process_music ();

private:
  Spanner *span_;
  Spanner *finished_;
  Stream_event *current_event_;
  Drul_array<Stream_event *> event_drul_;
  vector<Item *> note_columns_;
  void typeset_all ();
};

Episema_engraver::Episema_engraver (Context *c)
  : Engraver (c)
{
  finished_ = 0;
  current_event_ = 0;
  span_ = 0;
}

void
Episema_engraver::listen_episema (Stream_event *ev)
{
  Direction d = from_scm<Direction> (get_property (ev, "span-direction"));
  // Must not ASSIGN_EVENT_ONCE here, since episema
  // can be typeset over a single neume
  event_drul_[d] = ev;
}

void
Episema_engraver::process_music ()
{
  if (event_drul_[START])
    {
      if (current_event_)
        event_drul_[START]->warning (_ ("already have an episema"));
      else
        {
          current_event_ = event_drul_[START];
          span_ = make_spanner ("Episema", event_drul_[START]->self_scm ());

          event_drul_[START] = 0;
        }
    }
  if (event_drul_[STOP])
    {
      if (!span_)
        event_drul_[STOP]->warning (_ ("cannot find start of episema"));
      else
        {
          finished_ = span_;
          announce_end_grob (finished_, SCM_EOL);
          span_ = 0;
          current_event_ = 0;
          note_columns_.clear ();
        }
    }
}

void
Episema_engraver::typeset_all ()
{
  if (finished_)
    {
      if (!finished_->get_bound (RIGHT))
        {
          auto *col = (!note_columns_.empty ()
                       ? note_columns_.back ()
                       : unsmob<Item> (get_property (this,
                                                     "currentMusicalColumn")));
          finished_->set_bound (RIGHT, col);
        }
      finished_ = 0;
    }
}

void
Episema_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      auto *col = (!note_columns_.empty ()
                   ? note_columns_.front ()
                   : unsmob<Item> (get_property (this,
                                                 "currentMusicalColumn")));
      span_->set_bound (LEFT, col);
    }

  typeset_all ();
  event_drul_ = {};
}

void
Episema_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    {
      current_event_->warning (_ ("unterminated episema"));
      span_->suicide ();
      span_ = 0;
    }
}

void
Episema_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  note_columns_.push_back (info.grob ());
}

void
Episema_engraver::acknowledge_note_head (Grob_info info)
{
  if (span_)
    {
      Side_position_interface::add_support (span_, info.grob ());
      add_bound_item (span_, info.grob ());
    }
  else if (finished_)
    {
      Side_position_interface::add_support (finished_, info.grob ());
      add_bound_item (finished_, info.grob ());
    }
}

void
Episema_engraver::boot ()
{
  ADD_LISTENER (Episema_engraver, episema);
  ADD_ACKNOWLEDGER (Episema_engraver, note_column);
  ADD_ACKNOWLEDGER (Episema_engraver, note_head);
}

ADD_TRANSLATOR (Episema_engraver,
                /* doc */
                R"(
Create an @emph{Editio Vaticana}-style episema line.
                )",

                /* create */
                R"(
Episema
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
