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
   perform span-dynamics
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
  virtual void do_post_move_processing ();

private:
  Audio_dynamic* audio_p_;
  Span_req* span_start_req_l_;
  Drul_array<Span_req*> span_req_l_drul_;
  Array<Audio_dynamic_tuple> dynamic_tuple_arr_;
  Array<Audio_dynamic_tuple> finished_dynamic_tuple_arr_;
  Direction finished_dir_;
};

ADD_THIS_TRANSLATOR (Span_dynamic_performer);

Span_dynamic_performer::Span_dynamic_performer ()
{
  span_req_l_drul_[START] = 0;
  span_req_l_drul_[STOP] = 0;
  span_start_req_l_ = 0;
  audio_p_ = 0;
}

void
Span_dynamic_performer::acknowledge_element (Audio_element_info i)
{
  if (Audio_dynamic * d = dynamic_cast <Audio_dynamic*> (i.elem_l_))
    {
      Audio_dynamic_tuple a = { d, now_mom () };
      if (!span_req_l_drul_[START])
	dynamic_tuple_arr_.clear ();
      dynamic_tuple_arr_.push (a);
      if (finished_dynamic_tuple_arr_.size ())
	finished_dynamic_tuple_arr_.push (a);
    }
}

void
Span_dynamic_performer::do_process_requests ()
{
  if (finished_dynamic_tuple_arr_.size () > 1
     && finished_dynamic_tuple_arr_.top ().audio_l_->volume_i_)
    {
      Real start_volume = finished_dynamic_tuple_arr_[0].audio_l_->volume_i_;
      Real dv = finished_dynamic_tuple_arr_.top ().audio_l_->volume_i_
	- start_volume;
      /*
	urg.
	Catch and fix the case of:

             |                         |
	    x|                        x|
            f cresc.  -- -- -- -- --  pp 

	 Actually, we should provide a non-displayed dynamic/volume setting,
	 to set volume to 'ff' just before the pp.
       */
      if (!dv || sign (dv) != finished_dir_)
	{
	  // urg.  about one volume step
	  dv = (int)finished_dir_ * 13;
	  if (!start_volume)
	    start_volume = finished_dynamic_tuple_arr_.top
	      ().audio_l_->volume_i_ - dv;
	}
      Moment start_mom = finished_dynamic_tuple_arr_[0].mom_;
      Moment dt = finished_dynamic_tuple_arr_.top ().mom_ - start_mom;
      for (int i=0; i < finished_dynamic_tuple_arr_.size (); i++)
	{
	  Audio_dynamic_tuple* a = &finished_dynamic_tuple_arr_[i];
	  Real volume = start_volume + dv * (Real)(a->mom_ - start_mom)
	    / (Real)dt;
	  a->audio_l_->volume_i_ = (int)volume;
	}
      finished_dynamic_tuple_arr_.clear ();
    }

  if (span_req_l_drul_[STOP])
    {
      if (!span_start_req_l_)
	{
	  span_req_l_drul_[STOP]->warning (_ ("can't find start of (de)crescendo"));
	}
      else
	{
	  span_start_req_l_ = 0;
	  finished_dynamic_tuple_arr_ = dynamic_tuple_arr_;
	  finished_dir_ = span_req_l_drul_[STOP]->span_type_str_ == "crescendo"
	    ? RIGHT : LEFT;
	  dynamic_tuple_arr_.clear ();
	  if (finished_dynamic_tuple_arr_.size ())
	    dynamic_tuple_arr_.push (finished_dynamic_tuple_arr_.top ());
	}
    }

  if (span_req_l_drul_[START])
    {
      span_start_req_l_ = span_req_l_drul_[START];
      audio_p_ = new Audio_dynamic (0);
      Audio_element_info info (audio_p_, 0);
      announce_element (info);

      Audio_dynamic_tuple a = { audio_p_, now_mom () };
      dynamic_tuple_arr_.push (a);
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

void
Span_dynamic_performer::do_post_move_processing ()
{
  span_req_l_drul_[STOP] = 0;
  span_req_l_drul_[START] = 0;
}

bool
Span_dynamic_performer::do_try_music (Music* r)
{
  if (Span_req * s = dynamic_cast<Span_req*>(r))
    {
      if (s-> span_type_str_ == "crescendo"
	  || s->span_type_str_ == "decrescendo")
	{
	  span_req_l_drul_[s->span_dir_] = s;
	  return true;
	}
    }
  return false;
}
