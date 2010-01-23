/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "listener.hh"
#include "moment.hh"
#include "stream-event.hh"

class Paper_column_engraver : public Engraver
{
  void make_columns ();
  void set_columns (Paper_column *, Paper_column *);
  TRANSLATOR_DECLARATIONS (Paper_column_engraver);

  Paper_column *find_turnable_column (Moment after_this);
  void revoke_page_turns (Moment after_this, Real new_penalty);

protected:
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
  virtual void initialize ();
  virtual void finalize ();

  DECLARE_TRANSLATOR_LISTENER (break);
  DECLARE_TRANSLATOR_LISTENER (label);

  DECLARE_ACKNOWLEDGER (item);
  DECLARE_ACKNOWLEDGER (note_spacing);
  DECLARE_ACKNOWLEDGER (staff_spacing);

  System *system_;
  vector<Stream_event*> break_events_;
  vector<Stream_event*> label_events_;
  int breaks_;			// used for stat printing
  Paper_column *command_column_;
  Paper_column *musical_column_;
  vector<Item*> items_;
  bool first_;
  Moment last_moment_;

public:
};

#endif /* PAPER_COLUMN_ENGRAVER_HH */
