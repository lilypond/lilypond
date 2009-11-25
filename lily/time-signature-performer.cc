/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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
public:
  TRANSLATOR_DECLARATIONS (Time_signature_performer);
  ~Time_signature_performer ();

protected:

  void stop_translation_timestep ();
  void process_music ();
  virtual void derived_mark () const;
  SCM prev_fraction_;
private:

  Audio_time_signature *audio_;
};

void
Time_signature_performer::derived_mark () const
{
  scm_gc_mark (prev_fraction_);
}

Time_signature_performer::Time_signature_performer ()
{
  prev_fraction_ = SCM_BOOL_F;
  audio_ = 0;
}

Time_signature_performer::~Time_signature_performer ()
{
}

void
Time_signature_performer::process_music ()
{
  SCM fr = get_property ("timeSignatureFraction");
  if (scm_is_pair (fr) && !ly_is_equal (fr, prev_fraction_))
    {
      prev_fraction_ = fr;
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
  if (audio_)
    {
      audio_ = 0;
    }
}

#include "translator.icc"

ADD_TRANSLATOR (Time_signature_performer,
		/* doc */
		"",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
