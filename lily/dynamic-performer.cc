/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "performer.hh"
#include "audio-item.hh"
#include "stream-event.hh"

#include "translator.icc"

class Dynamic_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Dynamic_performer);
protected:
  void stop_translation_timestep ();
  void process_music ();
  Real equalize_volume (Real);

  DECLARE_TRANSLATOR_LISTENER (decrescendo);
  DECLARE_TRANSLATOR_LISTENER (crescendo);
  DECLARE_TRANSLATOR_LISTENER (absolute_dynamic);
private:
  Stream_event *script_event_;
  Drul_array<Stream_event*> span_events_; 
  Drul_array<Direction> grow_dir_; 
  Real last_volume_;
  Audio_dynamic *absolute_;
  Audio_span_dynamic *span_dynamic_;
  Audio_span_dynamic *finished_span_dynamic_;
};

Dynamic_performer::Dynamic_performer ()
{
  last_volume_ = 0.5;
  script_event_ = 0;
  absolute_ = 0;
  span_events_[LEFT] = 
    span_events_[RIGHT] = 0;
  span_dynamic_ = 0;
  finished_span_dynamic_ = 0;
}

Real
Dynamic_performer::equalize_volume (Real volume)
{
  /*
    properties override default equaliser setting
  */
  SCM min = get_property ("midiMinimumVolume");
  SCM max = get_property ("midiMaximumVolume");
  if (scm_is_number (min) || scm_is_number (max))
    {
      Interval iv (0, 1);
      if (scm_is_number (min))
	iv[MIN] = scm_to_double (min);
      if (scm_is_number (max))
	iv[MAX] = scm_to_double (max);
      volume = iv[MIN] + iv.length () * volume;
    }
  else
    {
      /*
	urg, code duplication:: staff_performer
      */
      SCM s = get_property ("midiInstrument");

      if (!scm_is_string (s))
	s = get_property ("instrumentName");

      if (!scm_is_string (s))
	s = scm_from_locale_string ("piano");

      SCM eq = get_property ("instrumentEqualizer");
      if (ly_is_procedure (eq))
	s = scm_call_1 (eq, s);

      if (is_number_pair (s))
	{
	  Interval iv = ly_scm2interval (s);
	  volume = iv[MIN] + iv.length () * volume;
	}
    }
  return volume;
}


void
Dynamic_performer::process_music ()
{
  if (span_events_[STOP] || script_event_)
    {
      finished_span_dynamic_ = span_dynamic_;
      span_dynamic_ = 0;
    }

  if (span_events_[START])
    {
      span_dynamic_ = new Audio_span_dynamic ();
      announce_element (Audio_element_info (span_dynamic_, span_events_[START]));

      span_dynamic_->grow_dir_ = grow_dir_[START];
    }

  if (script_event_
      || span_dynamic_
      || finished_span_dynamic_)
    {
      absolute_ = new Audio_dynamic ();

      if (script_event_)
	{
	  SCM proc = get_property ("dynamicAbsoluteVolumeFunction");

	  SCM svolume = SCM_EOL;
	  if (ly_is_procedure (proc))
	    {
	      // urg
	      svolume = scm_call_1 (proc, script_event_->get_property ("text"));
	    }

	  Real volume = robust_scm2double (svolume, 0.5);

	  last_volume_
	    = absolute_->volume_ = equalize_volume (volume);
	}
      
      Audio_element_info info (absolute_, script_event_);
      announce_element (info);
    }


  if (span_dynamic_)
    span_dynamic_->add_absolute (absolute_);

  if (finished_span_dynamic_)
    finished_span_dynamic_->add_absolute (absolute_);
}

void
Dynamic_performer::stop_translation_timestep ()
{
  if (finished_span_dynamic_)
    {
      finished_span_dynamic_->render ();
      finished_span_dynamic_ = 0;
    }
  
  if (absolute_ && absolute_->volume_ < 0)
    {
      absolute_->volume_ = last_volume_;
    }
  else if (absolute_)
    {
      last_volume_ = absolute_->volume_;
    }
  
  absolute_ = 0;
  script_event_ = 0;
  span_events_[LEFT] = 
    span_events_[RIGHT] = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Dynamic_performer, decrescendo);
void
Dynamic_performer::listen_decrescendo (Stream_event *r)
{
  Direction d = to_dir (r->get_property ("span-direction"));
  span_events_[d] = r;
  grow_dir_[d] = SMALLER;
}

IMPLEMENT_TRANSLATOR_LISTENER (Dynamic_performer, crescendo);
void
Dynamic_performer::listen_crescendo (Stream_event *r)
{
  Direction d = to_dir (r->get_property ("span-direction"));
  span_events_[d] = r;
  grow_dir_[d] = BIGGER;
}

IMPLEMENT_TRANSLATOR_LISTENER (Dynamic_performer, absolute_dynamic);
void
Dynamic_performer::listen_absolute_dynamic (Stream_event *r)
{
  if (!script_event_)
    script_event_ = r;
}

ADD_TRANSLATOR (Dynamic_performer,
		/* doc */
		"",

		/* create */
		"",

		/* read */
		"dynamicAbsoluteVolumeFunction "
		"instrumentEqualizer "
		"midiMaximumVolume "
		"midiMinimumVolume "
		"midiInstrument ",

		/* write */
		""
		);
