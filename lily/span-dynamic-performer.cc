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
  virtual bool try_music (Music*);
  virtual void acknowledge_grob (Audio_element_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();

private:
  Audio_dynamic* audio_p_;
  Real last_volume_;
  Span_req* span_start_req_l_;
  Drul_array<Span_req*> span_req_l_drul_;
  Array<Audio_dynamic_tuple> dynamic_tuple_arr_;
  Array<Audio_dynamic_tuple> finished_dynamic_tuple_arr_;
  Direction dir_;
  Direction finished_dir_;
};

ADD_THIS_TRANSLATOR (Span_dynamic_performer);

Span_dynamic_performer::Span_dynamic_performer ()
{
  span_req_l_drul_[START] = 0;
  span_req_l_drul_[STOP] = 0;
  span_start_req_l_ = 0;
  audio_p_ = 0;
  last_volume_ = 0;
}

void
Span_dynamic_performer::acknowledge_grob (Audio_element_info i)
{
  if (Audio_dynamic * d = dynamic_cast <Audio_dynamic*> (i.elem_l_))
    {
      last_volume_ = d->volume_;
    }
}

void
Span_dynamic_performer::create_grobs ()
{
  if (span_start_req_l_ || span_req_l_drul_[START])
    {
      audio_p_ = new Audio_dynamic (0);
      Audio_element_info info (audio_p_, span_req_l_drul_[START]
			       ? span_req_l_drul_[START]
			       : span_req_l_drul_[STOP]);
      announce_element (info);
      Audio_dynamic_tuple a = { audio_p_, now_mom () };
      dynamic_tuple_arr_.push (a);
    }
  
  if (span_req_l_drul_[STOP])
    {
      if (!span_start_req_l_)
	{
	  span_req_l_drul_[STOP]->origin ()->warning (_ ("can't find start of (de)crescendo"));
	  span_req_l_drul_[STOP] = 0;
	}
      else
	{
	  finished_dir_ = dir_;
	  finished_dynamic_tuple_arr_ = dynamic_tuple_arr_;
	}
      dynamic_tuple_arr_.clear ();
      span_start_req_l_ = 0;
    }

  if (span_req_l_drul_[START])
    {
      String t = ly_scm2string (span_req_l_drul_[START]->get_mus_property ("span-type"));
      dir_ = (t == "crescendo")	? RIGHT : LEFT;
      span_start_req_l_ = span_req_l_drul_[START];
      
      dynamic_tuple_arr_.clear ();
      Audio_dynamic_tuple a = { audio_p_, now_mom () };
      dynamic_tuple_arr_.push (a);
    }


  if (span_req_l_drul_[STOP])
    { 
      finished_dynamic_tuple_arr_.top ().audio_l_->volume_ = last_volume_;
    }
  
  if (span_req_l_drul_[START])
    {
      dynamic_tuple_arr_[0].audio_l_->volume_ = last_volume_;
    }
  span_start_req_l_ = 0;
  span_req_l_drul_[START] = 0;
  span_req_l_drul_[STOP] = 0;
}

void
Span_dynamic_performer::stop_translation_timestep ()
{
  if (finished_dynamic_tuple_arr_.size () > 1)
    {
      Real start_volume = finished_dynamic_tuple_arr_[0].audio_l_->volume_;
      Real dv = finished_dynamic_tuple_arr_.top ().audio_l_->volume_
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
	  // urg.  20%: about two volume steps
	  dv = (Real)finished_dir_ * 0.2;
	  if (!start_volume)
	    start_volume = finished_dynamic_tuple_arr_.top
	      ().audio_l_->volume_ - dv;
	}
      Moment start_mom = finished_dynamic_tuple_arr_[0].mom_;
      Moment dt = finished_dynamic_tuple_arr_.top ().mom_ - start_mom;
      for (int i=0; i < finished_dynamic_tuple_arr_.size (); i++)
	{
	  Audio_dynamic_tuple* a = &finished_dynamic_tuple_arr_[i];
	  Real volume = start_volume + dv * (Real)(a->mom_ - start_mom)
	    / (Real)dt;
	  a->audio_l_->volume_ = volume;
	}
      finished_dynamic_tuple_arr_.clear ();
    }

  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}

void
Span_dynamic_performer::start_translation_timestep ()
{
  span_req_l_drul_[STOP] = 0;
  span_req_l_drul_[START] = 0;
}

bool
Span_dynamic_performer::try_music (Music* r)
{
  if (Span_req * s = dynamic_cast<Span_req*>(r))
    {
      String t =  ly_scm2string (s->get_mus_property ("span-type"));      
      if (t == "crescendo" || t == "decrescendo")
	{
	  span_req_l_drul_[s->get_span_dir()] = s;
	  return true;
	}
    }
  return false;
}
