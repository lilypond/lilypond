/*
  dynamic-performer.cc -- implement Dynamic_performer

  source file of the GNU LilyPond music typesetter

  (c) 2000--2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"

#include "event.hh"
#include "audio-item.hh"

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
  virtual bool try_music (Music* req);
  virtual void stop_translation_timestep ();
  virtual void create_audio_elements ();

private:
  Music* script_req_;
  Audio_dynamic* audio_;
};

Dynamic_performer::Dynamic_performer ()
{
  script_req_ = 0;
  audio_ = 0;
}

void
Dynamic_performer::create_audio_elements ()
{
  if (script_req_)
    {
      SCM proc = get_property ("dynamicAbsoluteVolumeFunction");

      SCM svolume  = SCM_EOL;
      if (ly_c_procedure_p (proc))
	{
	  // urg
	  svolume = scm_call_1 (proc, script_req_->get_property ("text"));
	}

      Real volume = robust_scm2double (svolume, 0.5); 

      /*
	properties override default equaliser setting
       */
      SCM min = get_property ("midiMinimumVolume");
      SCM max = get_property ("midiMaximumVolume");
      if (ly_c_number_p (min) || ly_c_number_p (max))
	{
	  Interval iv (0, 1);
	  if (ly_c_number_p (min))
	    iv[MIN] = ly_scm2double (min);
	  if (ly_c_number_p (max))
	    iv[MAX] = ly_scm2double (max);
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
	  if (ly_c_procedure_p (eq))
	    {
	      s = scm_call_1 (eq, s);
	    }

	  if (is_number_pair (s))
	    {
	      Interval iv = scm_to_interval (s);
	      volume = iv[MIN] + iv.length () * volume;
	    }
	}
      
      audio_ = new Audio_dynamic (volume);
      Audio_element_info info (audio_, script_req_);
      announce_element (info);
      script_req_ = 0;
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
Dynamic_performer::try_music (Music* r)
{
  if (!script_req_)
    {
      if (r->is_mus_type ("absolute-dynamic-event")) // fixme.
	{
	  script_req_ = r;
	  return true;
	}
    }
  return false;
}

ENTER_DESCRIPTION (Dynamic_performer,
		  /*descr*/		  "",
		  /* creats*/ "",
		  /* accepts */     "absolute-dynamic-event",
		  /* acks */ "",
		  /*reads */"dynamicAbsoluteVolumeFunction midiMaximumVolume midiMinimumVolume midiInstrument instrumentEqualizer",
		  /*writes*/"");
