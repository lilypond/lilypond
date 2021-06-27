/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2021 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "audio-item.hh"
#include "performer.hh"
#include "protected-scm.hh"

class Time_signature_performer : public Performer
{
  Audio_time_signature *audio_;
  SCM last_time_fraction_;
  SCM time_cause_;

protected:
  void derived_mark () const override;
  void stop_translation_timestep ();
  void process_music ();
public:
  TRANSLATOR_DECLARATIONS (Time_signature_performer);
  void listen_time_signature (Stream_event *);
};

void
Time_signature_performer::derived_mark () const
{
  scm_gc_mark (last_time_fraction_);
  scm_gc_mark (time_cause_);
}

Time_signature_performer::Time_signature_performer (Context *c)
  : Performer (c)
{
  audio_ = 0;
  time_cause_ = SCM_EOL;
  last_time_fraction_ = SCM_BOOL_F;
}

void
Time_signature_performer::listen_time_signature (Stream_event *ev)
{
  time_cause_ = ev->self_scm ();
}

void
Time_signature_performer::process_music ()
{
  if (audio_)
    return;

  SCM fr = get_property (this, "timeSignatureFraction");
  if (scm_is_pair (fr) && !ly_is_equal (fr, last_time_fraction_))
    {
      last_time_fraction_ = fr;
      int b = scm_to_int (scm_car (fr));
      int o = scm_to_int (scm_cdr (fr));

      audio_ = new Audio_time_signature (b, o);
      Audio_element_info info (audio_, 0);
      announce_element (info);
    }
}

void
Time_signature_performer::stop_translation_timestep ()
{
  audio_ = 0;
  time_cause_ = SCM_EOL;
}

#include "translator.icc"

void
Time_signature_performer::boot ()
{
  ADD_LISTENER (Time_signature_performer, time_signature);
}

ADD_TRANSLATOR (Time_signature_performer,
                /* doc */
                "",

                /* create */
                "",

                /* read */
                "timeSignatureFraction ",

                /* write */
                ""
               );
