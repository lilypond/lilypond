/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef SCORE_PERFORMER_HH
#define SCORE_PERFORMER_HH

#include "moment.hh"
#include "performer-group.hh"

/**
   Top level performer. Completely takes care of MIDI output
*/
class Score_performer : public Performer_group
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Translator_group, Score_performer);
  Performance *performance_;

  ~Score_performer ();
  Score_performer ();

protected:
  DECLARE_LISTENER (finish);
  DECLARE_LISTENER (prepare);
  DECLARE_LISTENER (one_time_step);

  /* Engraver_group_engraver interface */
  virtual void connect_to_context (Context *);
  virtual void disconnect_from_context ();
  virtual void initialize ();
  virtual void announce_element (Audio_element_info);
  virtual void derived_mark () const;
  virtual void acknowledge_audio_elements ();
private:
  void header (Midi_stream &);

  Audio_column *audio_column_;
  bool skipping_;
  Moment skip_start_mom_;
  Moment offset_mom_;
};

#endif // SCORE_PERFORMER_HH
