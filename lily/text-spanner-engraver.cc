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

class Text_spanner_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Text_spanner_engraver);

protected:
  void finalize () override;
  void listen_text_span (Stream_event *);
  void acknowledge_note_column (Grob_info_t<Item>);
  void stop_translation_timestep ();
  void process_music ();

private:
  Unique_span_event_listener text_span_listener_;
  Spanner *span_ = nullptr;
  Spanner *finished_ = nullptr;
  void typeset_all ();
};

Text_spanner_engraver::Text_spanner_engraver (Context *c)
  : Engraver (c)
{
}

void
Text_spanner_engraver::process_music ()
{
  if (auto *const ender = text_span_listener_.get_stop ())
    {
      if (!span_)
        ender->warning (_ ("cannot find start of text spanner"));
      else
        {
          finished_ = span_;
          announce_end_grob (finished_, SCM_EOL);
          span_ = nullptr;
        }
    }

  if (auto *const starter = text_span_listener_.get_start ())
    {
      if (span_)
        {
          starter->warning (_ ("already have a text spanner"));
          span_->warning (_ ("text spanner was started here"));
        }
      else
        {
          span_ = make_spanner ("TextSpanner", starter->self_scm ());

          SCM direction_sym = ly_symbol2scm ("direction");
          SCM d_scm = get_property (starter, direction_sym);
          if (auto d = from_scm<Direction> (d_scm))
            set_property (span_, direction_sym, d_scm);

          Side_position_interface::set_axis (span_, Y_AXIS);
        }
    }
}

void
Text_spanner_engraver::typeset_all ()
{
  if (finished_)
    {
      if (!finished_->get_bound (RIGHT))
        {
          Grob *e = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
          finished_->set_bound (RIGHT, e);
        }
      finished_ = nullptr;
    }
}

void
Text_spanner_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      Grob *e = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
      span_->set_bound (LEFT, e);
    }

  typeset_all ();
  text_span_listener_.reset ();
}

void
Text_spanner_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    {
      span_->warning (_ ("unterminated text spanner"));
      span_->suicide ();
      span_ = nullptr;
    }
}

void
Text_spanner_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  if (span_)
    {
      Pointer_group_interface::add_grob (span_, ly_symbol2scm ("note-columns"),
                                         info.grob ());
      if (!span_->get_bound (LEFT))
        add_bound_item (span_, info.grob ());
    }
  else if (finished_)
    {
      Pointer_group_interface::add_grob (
        finished_, ly_symbol2scm ("note-columns"), info.grob ());
      if (!finished_->get_bound (RIGHT))
        add_bound_item (finished_, info.grob ());
    }
}

void
Text_spanner_engraver::boot ()
{
  ADD_DELEGATE_LISTENER (text_span);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Text_spanner_engraver,
                /* doc */
                R"(
Create text spanner from an event.
                )",

                /* create */
                R"(
TextSpanner
                )",

                /* read */
                R"(
currentMusicalColumn
                )",

                /* write */
                R"(

                )");
