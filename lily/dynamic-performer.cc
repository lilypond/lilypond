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
  void do_print () const;
  virtual bool do_try_music (Music* req_l);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();

private:
  Text_script_req* text_script_req_l_;
  Audio_dynamic* audio_p_;
};

ADD_THIS_TRANSLATOR (Dynamic_performer);

Dynamic_performer::Dynamic_performer ()
{
  text_script_req_l_ = 0;
  audio_p_ = 0;
}

Dynamic_performer::~Dynamic_performer ()
{
}

void
Dynamic_performer::do_print () const
{
#ifndef NPRINT
  if (text_script_req_l_)
    text_script_req_l_->print ();
#endif
}

void
Dynamic_performer::do_process_music ()
{
  if (text_script_req_l_)
    {
      
      SCM s = scm_eval2
	(gh_list
	 (ly_symbol2scm ("dynamic-absolute-volume"),
	  ly_quote_scm (ly_str02scm (text_script_req_l_->text_str_.ch_C ())),
	  SCM_UNDEFINED),
	 SCM_EOL);
      Real volume = gh_scm2double (scm_eval2 (ly_symbol2scm ("dynamic-default-volume"), SCM_EOL));
      if (gh_number_p (s))
	volume = gh_scm2double (s);
      
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
	  s = get_property ("midiInstrument");
	  
	  if (!gh_string_p(s))
	    s = get_property ("instrument");
	  
	  if (!gh_string_p(s))
	    s = ly_str02scm ("piano");
	  
	  
	  s = scm_eval2 (gh_list (ly_symbol2scm ("instrument-equaliser"),
				 s, SCM_UNDEFINED),
			 SCM_EOL);
	  if (gh_pair_p (s))
	    {
	      Interval iv;
	      iv[MIN] = gh_scm2double (gh_car (s));
	      iv[MAX] = gh_scm2double (gh_cdr (s));
	      volume = iv[MIN] + iv.length () * volume;
	    }
	}
      
      audio_p_ = new Audio_dynamic (volume);
      Audio_element_info info (audio_p_, text_script_req_l_);
      announce_element (info);
      text_script_req_l_ = 0;
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
  if (!text_script_req_l_)
    {
      // urg, text script, style `dynamic' is how absolute dynamics appear
      if(Text_script_req* t = dynamic_cast <Text_script_req*> (r))
	{
	  if (t->style_str_ == "dynamic")
	    {
	      text_script_req_l_ = t;
	      return true;
	    }
	}
    }
  return false;
}

