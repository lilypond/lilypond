/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef PAPER_COLUMN_ENGRAVER_HH
#define PAPER_COLUMN_ENGRAVER_HH

#include "engraver.hh"
#include "moment.hh"
#include "stream-event.hh"

#include <vector>

class Paper_column_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Paper_column_engraver);

protected:
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void pre_process_music ();
  void process_music ();
  void handle_manual_breaks (bool);
  void initialize () override;
  void finalize () override;

  void listen_break (Stream_event *);
  void listen_label (Stream_event *);

  void acknowledge_item (Grob_info_t<Item>);
  void acknowledge_note_spacing (Grob_info);
  void acknowledge_staff_spacing (Grob_info);
  void acknowledge_break_alignment (Grob_info);

private:
  System *system_ = nullptr;
  std::vector<Stream_event *> break_events_;
  std::vector<Stream_event *> label_events_;
  int breaks_ = 0; // used for stat printing
  Paper_column *command_column_ = nullptr;
  Paper_column *musical_column_ = nullptr;
  std::vector<Item *> items_;
  bool skiptypesetting_at_start_of_timestep_ = false;
};

#endif /* PAPER_COLUMN_ENGRAVER_HH */
