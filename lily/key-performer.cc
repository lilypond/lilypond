/*
  key-performer.cc -- implement Key_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "command-request.hh"
#include "audio-item.hh"
#include "performer.hh"

class Key_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS(Translator);
  
  Key_performer();
  ~Key_performer();

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
  if (key_req_l_ &&
      gh_list_p (key_req_l_->get_mus_property ("pitch-alist")))
    {
      audio_p_ = new Audio_key (); // *key_req_l_->key_);
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

