/*
  tempo-performer.cc -- implement Tempo_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "command-request.hh"
#include "audio-item.hh"
#include "performer.hh"

class Tempo_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
  Tempo_performer ();
  ~Tempo_performer ();

protected:

  virtual bool try_music (Music* req_l);
  virtual void stop_translation_timestep ();
  virtual void create_audio_elements ();

private:
  Tempo_req* tempo_req_l_;
  Audio_tempo* audio_p_;
};

ADD_THIS_TRANSLATOR (Tempo_performer);

Tempo_performer::Tempo_performer ()
{
  tempo_req_l_ = 0;
  audio_p_ = 0;
}

Tempo_performer::~Tempo_performer ()
{
}


void
Tempo_performer::create_audio_elements ()
{
  if (tempo_req_l_)
    {

      SCM met = tempo_req_l_->get_mus_property ("metronome-count");
      Duration *d = unsmob_duration (tempo_req_l_->get_mus_property ("duration"));
      
      Rational r =  (d->length_mom () / Moment (1, 4) * Moment (gh_scm2int (met))).main_part_;
      
      audio_p_ = new Audio_tempo (int (r));

      Audio_element_info info (audio_p_, tempo_req_l_);
      announce_element (info);
      tempo_req_l_ = 0;
    }
}

void
Tempo_performer::stop_translation_timestep ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

bool
Tempo_performer::try_music (Music* req_l)
{
  if (tempo_req_l_)
    return false;

  if (Tempo_req *t =
      dynamic_cast <Tempo_req *> (req_l))
    {
      tempo_req_l_ = t;
      return true;
    }

  return false;
}

