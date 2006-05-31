/*
  dynamic-performer.cc -- implement Dynamic_performer

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"

#include "audio-item.hh"
#include "music.hh"
#include "translator.icc"

/*
  TODO:

  handle multiple events

  perform absolute (text) dynamics
*/
class Dynamic_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Dynamic_performer);
protected:
  virtual bool try_music (Music *event);
  void stop_translation_timestep ();
  void process_music ();

private:
  Music *script_event_;
  Audio_dynamic *audio_;
};

Dynamic_performer::Dynamic_performer ()
{
  script_event_ = 0;
  audio_ = 0;
}

void
Dynamic_performer::process_music ()
{
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
	    s = get_property ("instrument");

	  if (!scm_is_string (s))
	    s = scm_makfrom0str ("piano");

	  SCM eq = get_property ("instrumentEqualizer");
	  if (ly_is_procedure (eq))
	    s = scm_call_1 (eq, s);

	  if (is_number_pair (s))
	    {
	      Interval iv = ly_scm2interval (s);
	      volume = iv[MIN] + iv.length () * volume;
	    }
	}

      audio_ = new Audio_dynamic (volume);
      Audio_element_info info (audio_, script_event_);
      announce_element (info);
      script_event_ = 0;
    }
}

void
Dynamic_performer::stop_translation_timestep ()
{
  if (audio_)
    {
      play_element (audio_);
      audio_ = 0;
    }
}

bool
Dynamic_performer::try_music (Music *r)
{
  if (!script_event_)
    {
      if (r->is_mus_type ("absolute-dynamic-event")) // fixme.
	{
	  script_event_ = r;
	  return true;
	}
    }
  return false;
}

ADD_TRANSLATOR (Dynamic_performer,
		/* doc */ 		 "",
		/* create */ "",
		/* accept */ "absolute-dynamic-event",
		/* read */ "dynamicAbsoluteVolumeFunction midiMaximumVolume midiMinimumVolume midiInstrument instrumentEqualizer",
		/*writes*/"");
