/*
  piano-pedal-performer.cc -- implement Piano_pedal_performer

  source file of the GNU LilyPond music typesetter

  (c)  2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "audio-item.hh"

/**
   perform Piano pedals
 */
class Piano_pedal_performer : public Performer
{
  struct Pedal_info
  {
    char const *name_;
    Span_req* start_req_;
    Drul_array<Span_req*> req_l_drul_;
  };

public:
  TRANSLATOR_DECLARATIONS(Piano_pedal_performer);
  ~Piano_pedal_performer ();
  
protected:
  virtual void initialize ();
  virtual bool try_music (Music*);
  virtual void create_audio_elements ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();

private:
  Link_array<Audio_piano_pedal> audios_;
  Pedal_info * info_alist_;
};

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

  char * names [] = { "Sostenuto", "Sustain", "UnaCorda", 0  };
  char **np = names ;
  do
    {
      p->name_ = *np;
      p->req_l_drul_[START] = 0;
      p->req_l_drul_[STOP] = 0;
      p->start_req_ = 0;

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
	  if (!p->start_req_)
	    {
	      p->req_l_drul_[STOP]->origin ()->warning (_f ("can't find start of piano pedal: `%s'", String (p->name_)));
	    }
	  else
	    {
	      Audio_piano_pedal* a = new Audio_piano_pedal;
	      a->type_string_ = String (p->name_);
	      a->dir_ = STOP;
	      audios_.push (a);
	    }
	  p->start_req_ = 0;
	}

      if (p->req_l_drul_[START])
	{
	  p->start_req_ = p->req_l_drul_[START];
	  Audio_piano_pedal* a = new Audio_piano_pedal;
	  a->type_string_ = String (p->name_);
	  a->dir_ = START;
	  audios_.push (a);
	}
      p->req_l_drul_[START] = 0;
      p->req_l_drul_[STOP] = 0;
    }
}

void
Piano_pedal_performer::stop_translation_timestep ()
{
  for (int i=0; i< audios_.size (); i++)
    play_element (audios_[i]);
  audios_.clear ();
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
			   scm_makfrom0str (p->name_)) == SCM_BOOL_T)
	    {
	      p->req_l_drul_[s->get_span_dir ()] = s;
	      return true;
	    }
	}
    }
  return false;
}

ENTER_DESCRIPTION (Piano_pedal_performer, "","",
		   "general-music",
		   "","","" );
