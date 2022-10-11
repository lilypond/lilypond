/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  OVERRIDE_CLASS_NAME (Score_performer);
  Performance *performance_;

  ~Score_performer ();
  Score_performer ();

protected:
  void finish (SCM);
  void prepare (SCM);
  void one_time_step (SCM);

  /* Engraver_group_engraver interface */
  void connect_to_context (Context *) override;
  void disconnect_from_context () override;
  void initialize () override;
  void announce_element (Audio_element_info) override;
  void derived_mark () const override;
  void acknowledge_audio_elements () override;

private:
  Audio_column *audio_column_;
  bool skipping_;
  Moment skip_last_mom_;
  Moment offset_mom_;
};

#endif // SCORE_PERFORMER_HH
