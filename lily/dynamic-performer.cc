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
    handle span requests (crescendo/decrescendo)
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
      
      SCM s = scm_eval
	(gh_list
	 (ly_symbol2scm ("dynamic-absolute-volume"),
	  ly_quote_scm (ly_str02scm (text_script_req_l_->text_str_.ch_C ())),
	  SCM_UNDEFINED));
      int volume = gh_scm2int (ly_eval_str ("dynamic-default-volume"));
      if (gh_number_p (s))
	volume = gh_scm2int (s);

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

