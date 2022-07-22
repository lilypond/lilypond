/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef TIMING_TRANSLATOR_HH
#define TIMING_TRANSLATOR_HH

#include "moment.hh"
#include "translator.hh"

class Stream_event;

class Timing_translator : public Translator
{
public:
  TRANSLATOR_DECLARATIONS (Timing_translator);

protected:
  void initialize () override;
  void listen_alternative (Stream_event *);
  void listen_bar (Stream_event *);
  void listen_fine (Stream_event *);
  void process_music ();
  void stop_translation_timestep ();
  void start_translation_timestep ();

private:
  bool measure_started_ = false;

  // alt_... members pertain to bar numbering for repeat alternatives
  Stream_event *alt_event_ = nullptr;
  int alt_starting_bar_number_ = 0;
  long alt_number_ = 0;
  long alt_number_increment_ = 0;
  bool alt_reset_enabled_ = false;

  Stream_event *fine_event_ = nullptr;
};

#endif // TIMING_TRANSLATOR_HH
