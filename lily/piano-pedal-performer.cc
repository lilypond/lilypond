/*
  piano-pedal-performer.cc -- implement Piano_pedal_performer

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"

#include "audio-item.hh"
#include "international.hh"
#include "music.hh"

/**
   perform Piano pedals
*/
class Piano_pedal_performer : public Performer
{
  struct Pedal_info
  {
    char const *name_;
    Music *start_event_;
    Drul_array<Music *> event_drul_;
  };

public:
  TRANSLATOR_DECLARATIONS (Piano_pedal_performer);
  ~Piano_pedal_performer ();

protected:
  virtual void initialize ();
  virtual bool try_music (Music *);
  void process_music ();
  void stop_translation_timestep ();
  void start_translation_timestep ();

private:
  Link_array__Audio_piano_pedal_ audios_;
  Pedal_info *info_alist_;
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

  char *names [] = { "Sostenuto", "Sustain", "UnaCorda", 0 };
  char **np = names;
  do
    {
      p->name_ = *np;
      p->event_drul_[START] = 0;
      p->event_drul_[STOP] = 0;
      p->start_event_ = 0;

      p++;
    }
  while (* (np++));
}

void
Piano_pedal_performer::process_music ()
{
  for (Pedal_info *p = info_alist_; p && p->name_; p++)

    {
      if (p->event_drul_[STOP])
	{
	  if (!p->start_event_)
	    p->event_drul_[STOP]->origin ()->warning (_f ("can't find start of piano pedal: `%s'", std::string (p->name_)));
	  else
	    {
	      Audio_piano_pedal *a = new Audio_piano_pedal;
	      a->type_string_ = std::string (p->name_);
	      a->dir_ = STOP;
	      audios_.push_back (a);
              Audio_element_info info(a, p->event_drul_[STOP]);
              announce_element (info);
	    }
	  p->start_event_ = 0;
	}

      if (p->event_drul_[START])
	{
	  p->start_event_ = p->event_drul_[START];
	  Audio_piano_pedal *a = new Audio_piano_pedal;
	  a->type_string_ = std::string (p->name_);
	  a->dir_ = START;
	  audios_.push_back (a);
          Audio_element_info info(a, p->event_drul_[START]);
          announce_element (info);
	}
      p->event_drul_[START] = 0;
      p->event_drul_[STOP] = 0;
    }
}

void
Piano_pedal_performer::stop_translation_timestep ()
{
  for (vsize i = 0; i < audios_.size (); i++)
    play_element (audios_[i]);
  audios_.clear ();
}

void
Piano_pedal_performer::start_translation_timestep ()
{
  for (Pedal_info *p = info_alist_; p && p->name_; p++)
    {
      p->event_drul_[STOP] = 0;
      p->event_drul_[START] = 0;
    }
}

bool
Piano_pedal_performer::try_music (Music *r)
{
  if (r->is_mus_type ("pedal-event"))
    {
      for (Pedal_info *p = info_alist_; p->name_; p++)
	{
	  std::string nm = p->name_ + std::string ("Event");
	  if (ly_is_equal (r->get_property ("name"),
			   scm_str2symbol (nm.c_str ())))
	    {
	      Direction d = to_dir (r->get_property ("span-direction"));
	      p->event_drul_[d] = r;
	      return true;
	    }
	}
    }
  return false;
}

#include "translator.icc"

ADD_TRANSLATOR (Piano_pedal_performer, "", "",
		"pedal-event",
		"", "");
