/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys

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
#include "item.hh"
#include "moment.hh"
#include "spanner.hh"
#include "stream-event.hh"

#include "translator.icc"

class Bend_engraver final : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Bend_engraver);
  void acknowledge_note_head (Grob_info);

protected:
  void listen_bend_after (Stream_event *);
  void finalize () override;
  void process_music ();
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void stop_fall ();

private:
  Moment stop_moment_;
  Stream_event *fall_event_ = nullptr;
  Spanner *fall_ = nullptr;
  Spanner *last_fall_ = nullptr;
  Grob *note_head_ = nullptr;
};

void
Bend_engraver::finalize ()
{
  // We shouldn't end a spanner on the last musical column of a piece because then
  // it would extend past the last breakable column of the piece.
  if (last_fall_)
    {
      auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      last_fall_->set_bound (RIGHT, col);
    }
}

void
Bend_engraver::stop_fall ()
{
  last_fall_ = fall_;
  fall_ = 0;
  note_head_ = 0;
  fall_event_ = 0;
}

void
Bend_engraver::stop_translation_timestep ()
{
  if (last_fall_)
    {
      // don't cross a barline
      SCM col_scm = unsmob<Grob> (get_property (this, "currentBarLine"))
                      ? get_property (this, "currentCommandColumn")
                      : get_property (this, "currentMusicalColumn");
      if (auto *col = unsmob<Grob> (col_scm))
        {
          last_fall_->set_bound (RIGHT, col);
        }
    }

  if (fall_ && !fall_->get_bound (LEFT))
    {
      fall_->set_bound (LEFT, note_head_);
      fall_->set_y_parent (note_head_);
    }
}

void
Bend_engraver::start_translation_timestep ()
{
  last_fall_ = 0;

  if (fall_ && now_mom ().main_part_ >= stop_moment_.main_part_)
    {
      stop_fall ();
    }
}

void
Bend_engraver::acknowledge_note_head (Grob_info info)
{
  if (!fall_event_)
    return;

  if (note_head_ && fall_)
    {
      stop_fall ();
    }

  note_head_ = info.grob ();
  stop_moment_
    = now_mom () + get_event_length (info.event_cause (), now_mom ());
}

Bend_engraver::Bend_engraver (Context *c)
  : Engraver (c)
{
}

void
Bend_engraver::listen_bend_after (Stream_event *ev)
{
  assign_event_once (fall_event_, ev);
}

void
Bend_engraver::process_music ()
{
  if (fall_event_ && !fall_)
    {
      fall_ = make_spanner ("BendAfter", fall_event_->self_scm ());
      set_property (fall_, "delta-position",
                    to_scm (from_scm<double> (
                      get_property (fall_event_, "delta-step"), 0)));
    }
}

void
Bend_engraver::boot ()
{
  ADD_LISTENER (bend_after);
  ADD_ACKNOWLEDGER (note_head);
}

ADD_TRANSLATOR (Bend_engraver,
                /* doc */
                R"(
Create fall spanners.
                )",

                /* create */
                R"(
BendAfter
                )",

                /* read */
                R"(
currentBarLine
currentCommandColumn
currentMusicalColumn
                )",

                /* write */
                R"(

                )");
