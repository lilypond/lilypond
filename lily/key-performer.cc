/*
  key-performer.cc -- implement Key_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lily-guile.hh"
#include "command-request.hh"
#include "audio-item.hh"
#include "performer.hh"

class Key_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
  Key_performer ();
  ~Key_performer ();

protected:
  virtual bool try_music (Music* req_l);
  virtual void create_audio_elements ();
  virtual void stop_translation_timestep ();

private:
  Key_change_req* key_req_l_;
  Audio_key* audio_p_;
};

ADD_THIS_TRANSLATOR (Key_performer);

Key_performer::Key_performer ()
{
  key_req_l_ = 0;
  audio_p_ = 0;
}

Key_performer::~Key_performer ()
{
}

void
Key_performer::create_audio_elements ()
{
  if (key_req_l_) 
    {
      SCM pitchlist = key_req_l_->get_mus_property ("pitch-alist");
      SCM proc = scm_primitive_eval (ly_symbol2scm ("accidentals-in-key")); 
      SCM acc = gh_call1 (proc, pitchlist);
      proc = scm_primitive_eval (ly_symbol2scm ("major-key"));
      SCM major = gh_call1 (proc, pitchlist);
      audio_p_ = new Audio_key (gh_scm2int (acc), major == SCM_BOOL_T); 
      Audio_element_info info (audio_p_, key_req_l_);
      announce_element (info);
      key_req_l_ = 0;
    }
}

void
Key_performer::stop_translation_timestep ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

bool
Key_performer::try_music (Music* req_l)
{
  if (Key_change_req *kc = dynamic_cast <Key_change_req *> (req_l))
    {
      if (key_req_l_)
	warning (_ ("FIXME: key change merge"));

      key_req_l_ = kc;
      return true;
    }

  return false;
}

