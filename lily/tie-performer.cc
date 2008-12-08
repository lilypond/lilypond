/*
  tie-performer.cc -- implement Tie_performer

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "performer.hh"

#include "audio-item.hh"
#include "context.hh"
#include "stream-event.hh"
#include "translator.icc"

class Tie_performer : public Performer
{
  Stream_event *event_;
  vector<Audio_element_info> now_heads_;
  vector<Audio_element_info> now_tied_heads_;
  vector<Audio_element_info> heads_to_tie_;

  bool ties_created_;

protected:
  void stop_translation_timestep ();
  void start_translation_timestep ();
  virtual void acknowledge_audio_element (Audio_element_info);
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (tie);
public:
  TRANSLATOR_DECLARATIONS (Tie_performer);
};

Tie_performer::Tie_performer ()
{
  event_ = 0;
  ties_created_ = false;
}

IMPLEMENT_TRANSLATOR_LISTENER (Tie_performer, tie);
void
Tie_performer::listen_tie (Stream_event *ev)
{
  event_ = ev;
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
      if (an->tie_event_)
        now_tied_heads_.push_back (inf);
      else
        now_heads_.push_back (inf);

      for (vsize i = heads_to_tie_.size (); i--;)
	{
	  Stream_event *right_mus = inf.event_;

	  Audio_note *th = dynamic_cast<Audio_note *> (heads_to_tie_[i].elem_);
	  Stream_event *left_mus = heads_to_tie_[i].event_;

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
      ties_created_ = false;
    }

  if (event_)
    {
      heads_to_tie_ = now_heads_;
    }

  for (vsize i = now_tied_heads_.size (); i--;)
    heads_to_tie_.push_back (now_tied_heads_[i]);

  event_ = 0;
  now_heads_.clear ();
  now_tied_heads_.clear ();
}

ADD_TRANSLATOR (Tie_performer,
		/* doc */
		"Generate ties between note heads of equal pitch.",

		/* create */
		"",

		/* read */
		"tieMelismaBusy",

		/* write */
		""
		);
