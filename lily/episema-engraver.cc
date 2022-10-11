/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Neil Puttock <n.puttock@gmail.com>

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
#include "span-event-listener.hh"
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
  // Must not use Unique_span_event_listener since episema can be typeset over a
  // single neume.
  Last_span_event_listener episema_listener_;
  Spanner *span_ = nullptr;
  Spanner *finished_ = nullptr;
  vector<Item *> note_columns_;
  void typeset_all ();
};

Episema_engraver::Episema_engraver (Context *c)
  : Engraver (c)
{
}

void
Episema_engraver::process_music ()
{
  if (auto *const starter = episema_listener_.get_start ())
    {
      if (span_)
        {
          starter->warning (_ ("already have an episema"));
          span_->warning (_ ("episema was started here"));
        }
      else
        {
          span_ = make_spanner ("Episema", starter->self_scm ());
        }
    }

  if (auto *const ender = episema_listener_.get_stop ())
    {
      if (!span_)
        ender->warning (_ ("cannot find start of episema"));
      else
        {
          finished_ = span_;
          announce_end_grob (finished_, SCM_EOL);
          span_ = nullptr;
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
          auto *col
            = (!note_columns_.empty ()
                 ? note_columns_.back ()
                 : unsmob<Item> (get_property (this, "currentMusicalColumn")));
          finished_->set_bound (RIGHT, col);
        }
      finished_ = nullptr;
    }
}

void
Episema_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      auto *col
        = (!note_columns_.empty ()
             ? note_columns_.front ()
             : unsmob<Item> (get_property (this, "currentMusicalColumn")));
      span_->set_bound (LEFT, col);
    }

  typeset_all ();
  episema_listener_.reset ();
}

void
Episema_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    {
      span_->warning (_ ("unterminated episema"));
      span_->suicide ();
      span_ = nullptr;
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
  ADD_DELEGATE_LISTENER (episema);
  ADD_ACKNOWLEDGER (note_column);
  ADD_ACKNOWLEDGER (note_head);
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
