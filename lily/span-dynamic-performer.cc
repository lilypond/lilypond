/*
  span-dynamic-performer.cc -- implement Span_dynamic_performer

  source file of the GNU LilyPond music typesetter

  (c)  2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "audio-item.hh"

struct Audio_dynamic_tuple
{
  Audio_dynamic* audio_l_;
  Moment mom_;
};

/**
   handle perform span-dynamics
 */
class Span_dynamic_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
  Span_dynamic_performer ();

protected:
  virtual bool do_try_music (Music* req_l);
  virtual void acknowledge_element (Audio_element_info);
  virtual void do_process_requests ();
  virtual void do_pre_move_processing ();

private:
  Drul_array<Span_req*> request_drul_;
  Drul_array<Moment> moment_drul_;
  Drul_array<int> volume_drul_;
  Array<Audio_dynamic_tuple> dynamic_tuple_arr_;

  // BURP
  Drul_array<Moment> done_moment_drul_;
  Drul_array<int> done_volume_drul_;
  Array<Audio_dynamic_tuple> done_dynamic_tuple_arr_;

  Audio_dynamic* audio_p_;
};

ADD_THIS_TRANSLATOR (Span_dynamic_performer);

Span_dynamic_performer::Span_dynamic_performer ()
{
  request_drul_[START] = request_drul_[STOP] = 0;
  volume_drul_[START] = volume_drul_[STOP] = 0;
  audio_p_ = 0;
}

void
Span_dynamic_performer::acknowledge_element (Audio_element_info i)
{
  if (Audio_dynamic * d = dynamic_cast <Audio_dynamic*> (i.elem_l_))
    {
      Direction dir = volume_drul_[START] ? STOP : START;
      volume_drul_[dir] = d->volume_i_;
      if (done_dynamic_tuple_arr_.size ())
	done_volume_drul_[STOP] = d->volume_i_;
#if 0
      Audio_dynamic_tuple a = { d, now_mom () };
      dynamic_tuple_arr_.push (a);
#endif
    }
}

void
Span_dynamic_performer::do_process_requests ()
{
  if (request_drul_[START])
    {
      audio_p_ = new Audio_dynamic (volume_drul_[START]);
      Audio_element_info info (audio_p_, 0);
      announce_element (info);

      Audio_dynamic_tuple a = { audio_p_, now_mom () };
      dynamic_tuple_arr_.push (a);
    }

  if (done_dynamic_tuple_arr_.size ())
    {
      if (done_volume_drul_[STOP])
	{
	  Real dv = done_volume_drul_[STOP] - done_volume_drul_[START];
	  Moment dt = done_moment_drul_[STOP] - done_moment_drul_[START];
	  for (int i=0; i < done_dynamic_tuple_arr_.size (); i++)
	    {
	      Real volume =
		(done_volume_drul_[START]
		 + dv * (Real)(done_dynamic_tuple_arr_[i].mom_
			       - done_moment_drul_[START]) / (Real)dt);
	      done_dynamic_tuple_arr_[i].audio_l_->volume_i_ = (int)volume;
	    }
	}
      done_dynamic_tuple_arr_.clear ();
    }

  if (request_drul_[STOP])
    {
      done_dynamic_tuple_arr_ = dynamic_tuple_arr_;
      dynamic_tuple_arr_.clear ();
      done_volume_drul_[START] = volume_drul_[START];
      done_volume_drul_[STOP] = volume_drul_[STOP];
      done_moment_drul_[START] = moment_drul_[START];
      done_moment_drul_[STOP] = moment_drul_[STOP];
      request_drul_[STOP] = 0;
      volume_drul_[START] = volume_drul_[STOP];
      volume_drul_[STOP] = 0;
    }
}

void
Span_dynamic_performer::do_pre_move_processing ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

bool
Span_dynamic_performer::do_try_music (Music* r)
{
  if (Span_req * s = dynamic_cast<Span_req*>(r))
    {
      if (s-> span_type_str_ != "crescendo"
	  && s->span_type_str_ != "decrescendo")
	return false;
      
      Direction d = s->span_dir_;

      if (d == STOP && !request_drul_[START])
	{
	  r->warning (_ ("No (de)crescendo to end"));
	  return false;
	}
      request_drul_[d] = s;
      moment_drul_[d] = now_mom ();
      if (d == START && volume_drul_[STOP])
	volume_drul_[START] = volume_drul_[STOP];
      return true;
    }
  return false;
}
