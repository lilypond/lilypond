/*
  piano-pedal-performer.cc -- implement Piano_pedal_performer

  source file of the GNU LilyPond music typesetter

  (c)  2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "audio-item.hh"
#include "dictionary.hh"
#include "dictionary-iter.hh"

/**
   perform Piano pedals
 */
class Piano_pedal_performer : public Performer
{
  struct Pedal_info
  {
    Span_req* start_req_l_;
    Drul_array<Span_req*> req_l_drul_;
  };

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
  Dictionary<Pedal_info> info_dict_;
};

ADD_THIS_TRANSLATOR (Piano_pedal_performer);

Piano_pedal_performer::Piano_pedal_performer ()
{
  (void)info_dict_["Sostenuto"];
  (void)info_dict_["Sustain"];
  (void)info_dict_["UnaChorda"];
  for (Dictionary_iter <Pedal_info> i (info_dict_); i.ok (); i++)
    {
      Pedal_info& p = i.val_ref ();
      p.req_l_drul_[START] = 0;
      p.req_l_drul_[STOP] = 0;
      p.start_req_l_ = 0;
    }
}

void
Piano_pedal_performer::do_process_music ()
{
  for (Dictionary_iter <Pedal_info> i (info_dict_); i.ok (); i++)
    {
      Pedal_info& p = i.val_ref ();
      if (p.req_l_drul_[STOP])
	{
	  if (!p.start_req_l_)
	    {
	      p.req_l_drul_[STOP]->warning (_f ("can't find start of piano pedal: %s", i.key ()));
	    }
	  else
	    {
	      Audio_piano_pedal* a = new Audio_piano_pedal;
	      a->type_str_ = i.key ();
	      a->dir_ = STOP;
	      audio_p_arr_.push (a);
	    }
	  p.start_req_l_ = 0;
	}

      if (p.req_l_drul_[START])
	{
	  p.start_req_l_ = p.req_l_drul_[START];
	  Audio_piano_pedal* a = new Audio_piano_pedal;
	  a->type_str_ = i.key ();
	  a->dir_ = START;
	  audio_p_arr_.push (a);
	}
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
  for (Dictionary_iter <Pedal_info> i (info_dict_); i.ok (); i++)
    {
      Pedal_info& p = i.val_ref ();
      p.req_l_drul_[STOP] = 0;
      p.req_l_drul_[START] = 0;
    }
}

bool
Piano_pedal_performer::do_try_music (Music* r)
{
  for (Dictionary_iter <Pedal_info> i (info_dict_); i.ok (); i++)
    {
      Pedal_info& p = i.val_ref ();
      if (Span_req * s = dynamic_cast<Span_req*>(r))
	{
	  if (s->span_type_str_ == i.key ())
	    {
	      p.req_l_drul_[s->span_dir_] = s;
	      return true;
	    }
	}
    }
  return false;
}
