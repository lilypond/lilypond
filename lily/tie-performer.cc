/*
  tie-performer.cc -- implement Tie_performer

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "performer.hh"

#include "music.hh"
#include "context.hh"
#include "audio-item.hh"
#include "pqueue.hh"

class Tie_performer : public Performer
{
  Music *event_;
  Music *last_event_;
  vector<Audio_element_info> now_heads_;
  vector<Audio_element_info> heads_to_tie_;

  bool ties_created_;

protected:
  void stop_translation_timestep ();
  void start_translation_timestep ();
  virtual void acknowledge_audio_element (Audio_element_info);
  virtual bool try_music (Music *);
  void process_music ();
public:
  TRANSLATOR_DECLARATIONS (Tie_performer);
};

Tie_performer::Tie_performer ()
{
  event_ = 0;
  last_event_ = 0;
  ties_created_ = false;
}

bool
Tie_performer::try_music (Music *mus)
{
  if (mus->is_mus_type ("tie-event"))
    event_ = mus;

  return true;
}

void
Tie_performer::process_music ()
{
  if (event_)
    context ()->set_property ("tieMelismaBusy", SCM_BOOL_T);
}

void
Tie_performer::acknowledge_audio_element (Audio_element_info inf)
{
  if (Audio_note *an = dynamic_cast<Audio_note *> (inf.elem_))
    {
      now_heads_.push_back (inf);
      for (vsize i = heads_to_tie_.size (); i--;)
	{
	  Music *right_mus = inf.event_;

	  Audio_note *th = dynamic_cast<Audio_note *> (heads_to_tie_[i].elem_);
	  Music *left_mus = heads_to_tie_[i].event_;

	  if (right_mus && left_mus
	      && ly_is_equal (right_mus->get_property ("pitch"),
			      left_mus->get_property ("pitch")))
	    {
	      an->tie_to (th);
	      ties_created_ = true;
	    }
	}
    }
}

void
Tie_performer::start_translation_timestep ()
{
  context ()->set_property ("tieMelismaBusy",
			    ly_bool2scm (heads_to_tie_.size ()));
}

void
Tie_performer::stop_translation_timestep ()
{
  if (ties_created_)
    {
      heads_to_tie_.clear ();
      last_event_ = 0;
      ties_created_ = false;
    }

  if (event_)
    {
      heads_to_tie_ = now_heads_;
      last_event_ = event_;
    }
  event_ = 0;
  now_heads_.clear ();
}

#include "translator.icc"

ADD_TRANSLATOR (Tie_performer,
		/* doc */ "Generate ties between noteheads of equal pitch.",
		/* create */ "",
		/* accept */ "tie-event",
		/* read */ "tieMelismaBusy",
		/* write */ "");
