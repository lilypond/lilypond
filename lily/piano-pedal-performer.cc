/*
 piano-pedal-performer.cc -- implement Piano_pedal_performer

  source file of the GNU LilyPond music typesetter

  (c)  2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "audio-item.hh"


/*
  TODO:
    sostenuto
    una-chorda ?
 */

/**
   perform Piano pedals
 */
class Piano_pedal_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
  Piano_pedal_performer ();

protected:
  virtual bool do_try_music (Music*);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();

private:
  Link_array<Audio_piano_pedal> audio_p_arr_;
  Span_req* span_start_req_l_;
  Drul_array<Span_req*> span_req_l_drul_;
};

ADD_THIS_TRANSLATOR (Piano_pedal_performer);

Piano_pedal_performer::Piano_pedal_performer ()
{
  span_req_l_drul_[START] = 0;
  span_req_l_drul_[STOP] = 0;
  span_start_req_l_ = 0;
}

void
Piano_pedal_performer::do_process_music ()
{
  if (span_req_l_drul_[STOP])
    {
      if (!span_start_req_l_)
	{
	  span_req_l_drul_[STOP]->warning (_ ("can't find start of piano_pedal"));
	}
      else
	{
	  Audio_piano_pedal* p = new Audio_piano_pedal;
	  p->type_b_ = false;
	  audio_p_arr_.push (p);
	}
      span_start_req_l_ = 0;
    }

  if (span_req_l_drul_[START])
    {
      span_start_req_l_ = span_req_l_drul_[START];
      Audio_piano_pedal* p = new Audio_piano_pedal;
      p->type_b_ = true;
      audio_p_arr_.push (p);
    }
}

void
Piano_pedal_performer::do_pre_move_processing ()
{
  for (int i=0; i < audio_p_arr_.size (); i++)
    play_element (audio_p_arr_[i]);
  audio_p_arr_.clear ();
}

void
Piano_pedal_performer::do_post_move_processing ()
{
  span_req_l_drul_[STOP] = 0;
  span_req_l_drul_[START] = 0;
}

bool
Piano_pedal_performer::do_try_music (Music* r)
{
  if (Span_req * s = dynamic_cast<Span_req*>(r))
    {
      if (s-> span_type_str_ == "sustain")
	{
	  span_req_l_drul_[s->span_dir_] = s;
	  return true;
	}
    }
  return false;
}
