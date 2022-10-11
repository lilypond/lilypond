/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef MARK_ENGRAVER_HH
#define MARK_ENGRAVER_HH

#include "engraver.hh"

/**
   put stuff over or next to  bars.  Examples: bar numbers, marginal notes,
   rehearsal marks.
*/
class Mark_engraver : public Engraver
{
private:
  struct Mark_state
  {
    Item *text_ = nullptr;
    Item *final_text_ = nullptr;
  };

private:
  Mark_state performance_mark_state_;
  Mark_state rehearsal_mark_state_;
  bool first_time_ = true;

public:
  TRANSLATOR_DECLARATIONS (Mark_engraver);

  // Get the text property of the current performance mark in the given context
  // (SCM_EOL if there is no mark).
  static SCM get_current_performance_mark_text (Context *);

  // Get the text property of the current rehearsal mark in the given context
  // (SCM_EOL if there is no mark).
  static SCM get_current_rehearsal_mark_text (Context *);

protected:
  void process_music ();
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void finalize () override;

private:
  static SCM get_current_performance_mark (Context *context,
                                           const char **grob_name, SCM *text);
  static SCM get_current_rehearsal_mark (Context *context,
                                         const char **grob_name, SCM *text);
};

#endif /* MARK_TRACKING_TRANSLATOR_HH */
