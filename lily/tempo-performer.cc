/*
  tempo-performer.cc -- implement Tempo_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "command-request.hh"
#include "audio-item.hh"
#include "performer.hh"

class Tempo_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS(Translator);
  
  Tempo_performer();
  ~Tempo_performer();

protected:

  virtual bool do_try_music (Music* req_l);
  virtual void do_pre_move_processing ();
  virtual void process_acknowledged ();

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
Tempo_performer::process_acknowledged ()
{
  if (tempo_req_l_)
    {

      SCM met = tempo_req_l_->get_mus_property ("metronome-count");
      audio_p_ = new Audio_tempo (tempo_req_l_->dur_.length_mom () /
				  Moment (1, 4) 
				  * Moment(gh_scm2int (met)));

      Audio_element_info info (audio_p_, tempo_req_l_);
      announce_element (info);
      tempo_req_l_ = 0;
    }
}

void
Tempo_performer::do_pre_move_processing ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

bool
Tempo_performer::do_try_music (Music* req_l)
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

