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
  virtual void initialize ();
  virtual bool try_music (Music*);
  virtual void create_audio_elements ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();

private:
  Link_array<Audio_piano_pedal> audio_p_arr_;
  Pedal_info * info_alist_;
};

ADD_THIS_TRANSLATOR (Piano_pedal_performer);

Piano_pedal_performer::Piano_pedal_performer ()
{
  info_alist_ = 0;
}

Piano_pedal_performer::~Piano_pedal_performer ()
{
  delete[] info_alist_;
}

void
Piano_pedal_performer::initialize ()
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
  while (* (np ++));
}

void
Piano_pedal_performer::create_audio_elements ()
{
  for (Pedal_info*p = info_alist_; p && p->name_; p ++)
 
    {
      if (p->req_l_drul_[STOP])
	{
	  if (!p->start_req_l_)
	    {
	      p->req_l_drul_[STOP]->origin ()->warning (_f ("can't find start of piano pedal: %s", String (p->name_)));
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
      p->req_l_drul_[START] = 0;
      p->req_l_drul_[STOP] = 0;
    }
}

void
Piano_pedal_performer::stop_translation_timestep ()
{
  for (int i=0; i< audio_p_arr_.size (); i++)
    play_element (audio_p_arr_[i]);
  audio_p_arr_.clear ();
}

void
Piano_pedal_performer::start_translation_timestep ()
{
  for (Pedal_info*p = info_alist_; p && p->name_; p ++)
    {
      p->req_l_drul_[STOP] = 0;
      p->req_l_drul_[START] = 0;
    }
}

bool
Piano_pedal_performer::try_music (Music* r)
{
  if (Span_req * s = dynamic_cast<Span_req*> (r))
    {
      for (Pedal_info*p = info_alist_; p->name_; p ++)
	{
	  if (scm_equal_p (s->get_mus_property ("span-type"),
			   ly_str02scm (p->name_)) == SCM_BOOL_T)
	    {
	      p->req_l_drul_[s->get_span_dir ()] = s;
	      return true;
	    }
	}
    }
  return false;
}
