/*
  dynamic-performer.cc -- implement Dynamic_performer

  source file of the GNU LilyPond music typesetter

  (c)  2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "audio-item.hh"

/*
  TODO:
    handle multiple requests
 */

/**
   perform absolute (text) dynamics
 */
class Dynamic_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
  Dynamic_performer ();
  ~Dynamic_performer ();

protected:
  virtual bool do_try_music (Music* req_l);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();

private:
  Dynamic_script_req* script_req_l_;
  Audio_dynamic* audio_p_;
};

ADD_THIS_TRANSLATOR (Dynamic_performer);

Dynamic_performer::Dynamic_performer ()
{
  script_req_l_ = 0;
  audio_p_ = 0;
}

Dynamic_performer::~Dynamic_performer ()
{
}


void
Dynamic_performer::do_process_music ()
{
  if (script_req_l_)
    {
      SCM proc = get_property ("dynamicAbsoluteVolumeFunction");

      SCM svolume  = SCM_EOL;
      if (gh_procedure_p (proc))
	{
	  // urg
	  svolume = gh_call1 (proc, script_req_l_->get_mus_property ("text"));
	}

      Real volume = 0.5; 
      if (gh_number_p (svolume))
	volume = gh_scm2double (svolume);

      /*
	properties override default equaliser setting
       */
      SCM min = get_property ("midiMinimumVolume");
      SCM max = get_property ("midiMaximumVolume");
      if (gh_number_p (min) || gh_number_p (max))
	{
	  Interval iv (0, 1);
	  if (gh_number_p (min))
	    iv[MIN] = gh_scm2double (min);
	  if (gh_number_p (max))
	    iv[MAX] = gh_scm2double (max);
	  volume = iv[MIN] + iv.length () * volume;
	}
      else
	{
	  /*
	    urg, code duplication:: staff_performer
	  */
	  SCM s = get_property ("midiInstrument");
	  
	  if (!gh_string_p(s))
	    s = get_property ("instrument");
	  
	  if (!gh_string_p(s))
	    s = ly_str02scm ("piano");
	  
	  
	  SCM eq = get_property ("instrumentEqualizer");
	  if (gh_procedure_p (eq))
	    {
	      s = gh_call1 (eq, s);
	    }

	  if (gh_pair_p (s))
	    {
	      Interval iv;
	      iv[MIN] = gh_scm2double (gh_car (s));
	      iv[MAX] = gh_scm2double (gh_cdr (s));
	      volume = iv[MIN] + iv.length () * volume;
	    }
	}
      
      audio_p_ = new Audio_dynamic (volume);
      Audio_element_info info (audio_p_, script_req_l_);
      announce_element (info);
      script_req_l_ = 0;
    }
}

void
Dynamic_performer::do_pre_move_processing ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

bool
Dynamic_performer::do_try_music (Music* r)
{
  if (!script_req_l_)
    {
      if(Dynamic_script_req* d = dynamic_cast <Dynamic_script_req*> (r))
	{
	  script_req_l_ = d;
	  return true;
	}
    }
  return false;
}

