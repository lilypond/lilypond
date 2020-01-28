/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
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
  void acknowledge_note_column (Grob_info);
  void stop_translation_timestep ();
  void process_music ();

private:
  Spanner *span_;
  Spanner *finished_;
  Stream_event *current_event_;
  Drul_array<Stream_event *> event_drul_;
  void typeset_all ();
};

Text_spanner_engraver::Text_spanner_engraver (Context *c) : Engraver (c)
{
  finished_ = 0;
  current_event_ = 0;
  span_ = 0;
  event_drul_.set (0, 0);
}

void
Text_spanner_engraver::listen_text_span (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  ASSIGN_EVENT_ONCE (event_drul_[d], ev);
}

void
Text_spanner_engraver::process_music ()
{
  if (event_drul_[STOP])
    {
      if (!span_)
        event_drul_[STOP]->origin ()->warning (
            _ ("cannot find start of text spanner"));
      else
        {
          finished_ = span_;
          announce_end_grob (finished_, SCM_EOL);
          span_ = 0;
          current_event_ = 0;
        }
    }

  if (event_drul_[START])
    {
      if (current_event_)
        event_drul_[START]->origin ()->warning (
            _ ("already have a text spanner"));
      else
        {
          current_event_ = event_drul_[START];
          span_ = make_spanner ("TextSpanner", event_drul_[START]->self_scm ());
          if (Direction d = to_dir (current_event_->get_property ("direction")))
            span_->set_property ("direction", scm_from_int (d));

          Side_position_interface::set_axis (span_, Y_AXIS);
          event_drul_[START] = 0;
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
          Grob *e = unsmob<Grob> (get_property ("currentMusicalColumn"));
          finished_->set_bound (RIGHT, e);
        }
      finished_ = 0;
    }
}

void
Text_spanner_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      Grob *e = unsmob<Grob> (get_property ("currentMusicalColumn"));
      span_->set_bound (LEFT, e);
    }

  typeset_all ();
  event_drul_.set (0, 0);
}

void
Text_spanner_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    {
      current_event_->origin ()->warning (_ ("unterminated text spanner"));
      span_->suicide ();
      span_ = 0;
    }
}

void
Text_spanner_engraver::acknowledge_note_column (Grob_info info)
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
  ADD_LISTENER (Text_spanner_engraver, text_span);
  ADD_ACKNOWLEDGER (Text_spanner_engraver, note_column);
}

ADD_TRANSLATOR (Text_spanner_engraver,
                /* doc */
                "Create text spanner from an event.",

                /* create */
                "TextSpanner ",

                /* read */
                "currentMusicalColumn ",

                /* write */
                "");
