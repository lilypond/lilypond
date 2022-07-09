/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

/*
  C&P from text-spanner.cc

  - todo: ending should be detected automatically? a new note
  automatically is the end of the trill?
*/

#include "engraver.hh"

#include "international.hh"
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "stream-event.hh"
#include "spanner.hh"

#include "translator.icc"

class Trill_spanner_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Trill_spanner_engraver);
protected:
  void finalize () override;
  void listen_trill_span (Stream_event *);
  void acknowledge_note_column (Grob_info_t<Item>);

  void stop_translation_timestep ();
  void process_music ();

private:
  Spanner *span_ = nullptr;
  Spanner *finished_ = nullptr;
  Drul_array<Stream_event *> event_drul_;
};

Trill_spanner_engraver::Trill_spanner_engraver (Context *c)
  : Engraver (c)
{
}

void
Trill_spanner_engraver::listen_trill_span (Stream_event *ev)
{
  Direction d = from_scm<Direction> (get_property (ev, "span-direction"));
  assign_event_once (event_drul_[d], ev);
}

void
Trill_spanner_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  if (span_)
    {
      Pointer_group_interface::add_grob (span_,
                                         ly_symbol2scm ("note-columns"),
                                         info.grob ());
      if (!span_->get_bound (LEFT))
        add_bound_item (span_, info.grob ());
    }
  if (finished_)
    {
      Pointer_group_interface::add_grob (finished_, ly_symbol2scm ("note-columns"),
                                         info.grob ());
      if (!finished_->get_bound (RIGHT))
        add_bound_item (finished_, info.grob ());
    }
}

void
Trill_spanner_engraver::process_music ()
{
  if (span_)
    {
      auto *ender = event_drul_[STOP];
      if (!ender)
        ender = event_drul_[START];

      if (ender)
        {
          finished_ = span_;
          announce_end_grob (finished_, ender->self_scm ());
          span_ = nullptr;
        }
    }

  if (auto *const starter = event_drul_[START])
    {
      span_ = make_spanner ("TrillSpanner", starter->self_scm ());
      Side_position_interface::set_axis (span_, Y_AXIS);
      if (finished_)
        set_object (finished_, "right-neighbor", span_->self_scm ());
    }
}

void
Trill_spanner_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      Grob *e = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
      span_->set_bound (LEFT, e);
    }

  if (finished_)
    {
      if (!finished_->get_bound (RIGHT))
        {
          Grob *e = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
          finished_->set_bound (RIGHT, e);
        }
      finished_ = nullptr;
    }

  event_drul_ = {};
}

void
Trill_spanner_engraver::finalize ()
{
  if (span_)
    {
      Grob *e = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      span_->set_bound (RIGHT, e);
    }
}

void
Trill_spanner_engraver::boot ()
{
  ADD_LISTENER (trill_span);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Trill_spanner_engraver,
                /* doc */
                R"(
Create trill spanner from an event.
                )",

                /* create */
                R"(
TrillSpanner
                )",

                /* read */
                R"(
currentCommandColumn
currentMusicalColumn
                )",

                /* write */
                R"(

                )");
