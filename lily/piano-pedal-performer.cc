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
    char const *name_;
    Span_req* start_req_l_;
    Drul_array<Span_req*> req_l_drul_;
  };

public:
  VIRTUAL_COPY_CONS (Translator);
  Piano_pedal_performer ();
  ~Piano_pedal_performer ();
  
protected:
  virtual void do_creation_processing ();
  virtual bool do_try_music (Music*);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();

private:
  Link_array<Audio_piano_pedal> audio_p_arr_;
  Pedal_info * info_alist_;
};

ADD_THIS_TRANSLATOR (Piano_pedal_performer);

Piano_pedal_performer::Piano_pedal_performer ()
{
  info_alist_ = 0;
}

Piano_pedal_performer::~Piano_pedal_performer()
{
  delete[] info_alist_;
}

void
Piano_pedal_performer::do_creation_processing ()
{
  info_alist_ = new Pedal_info[4];
  Pedal_info *p = info_alist_;

  char * names [] = { "Sostenuto", "Sustain", "UnaChorda", 0  };
  char **np = names ;
  do
    {
      p->name_ = *np;
      p->req_l_drul_[START] = 0;
      p->req_l_drul_[STOP] = 0;
      p->start_req_l_ = 0;

      p++;
    }
  while (*(np ++));
}

void
Piano_pedal_performer::do_process_music ()
{
  for (Pedal_info*p = info_alist_; p->name_; p ++)
 
    {
      if (p->req_l_drul_[STOP])
	{
	  if (!p->start_req_l_)
	    {
	      p->req_l_drul_[STOP]->warning (_f ("can't find start of piano pedal: %s", String (p->name_)));
	    }
	  else
	    {
	      Audio_piano_pedal* a = new Audio_piano_pedal;
	      a->type_str_ = String (p->name_);
	      a->dir_ = STOP;
	      audio_p_arr_.push (a);
	    }
	  p->start_req_l_ = 0;
	}

      if (p->req_l_drul_[START])
	{
	  p->start_req_l_ = p->req_l_drul_[START];
	  Audio_piano_pedal* a = new Audio_piano_pedal;
	  a->type_str_ = String (p->name_);
	  a->dir_ = START;
	  audio_p_arr_.push (a);
	}
    }
}

void
Piano_pedal_performer::do_pre_move_processing ()
{
  for (int i=0; i< audio_p_arr_.size (); i++)
    play_element (audio_p_arr_[i]);
  audio_p_arr_.clear ();
}

void
Piano_pedal_performer::do_post_move_processing ()
{
  for (Pedal_info*p = info_alist_; p->name_; p ++)
    {
      p->req_l_drul_[STOP] = 0;
      p->req_l_drul_[START] = 0;
    }
}

bool
Piano_pedal_performer::do_try_music (Music* r)
{
  if (Span_req * s = dynamic_cast<Span_req*>(r))
    {
      for (Pedal_info*p = info_alist_; p->name_; p ++)
	{
	  if (s->span_type_str_ == String (p->name_))
	    {
	      p->req_l_drul_[s->span_dir_] = s;
	      return true;
	    }
	}
    }
  return false;
}
