/*
  tie-performer.cc -- implement Tie_performer

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "performer.hh"

#include "audio-item.hh"
#include "context.hh"
#include "stream-event.hh"
#include "translator.icc"
// #include "international.hh"
#include <deque>

struct Head_event_tuple
{
  Audio_element_info head_;
  Moment moment_;
  Head_event_tuple () { }
  Head_event_tuple (Audio_element_info h, Moment m)
  {
    head_ = h;
    moment_ = m;
  }
};


class Tie_performer : public Performer
{
  Stream_event *event_;
  // We don't really need a deque here. A vector would suffice. However,
  // for some strange reason, using vectors always leads to memory 
  // corruption in the STL templates! (i.e. after the first
  // now_heads_.push_back (inf_mom), the now_heads_.size() will be 
  // something like 3303820998 :(
  deque<Head_event_tuple> now_heads_;
  deque<Head_event_tuple> now_tied_heads_;
  deque<Head_event_tuple> heads_to_tie_;

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
//       message (_f ("acknowledge_audio_element, Size of now_heads_=%d", now_heads_.size ()));
      Head_event_tuple inf_mom (inf, now_mom ());
      if (an->tie_event_)
        now_tied_heads_.push_back (inf_mom);
      else
        now_heads_.push_back (inf_mom);

//       message (_f ("acknowledge_audio_element, added, Size of now_heads_=%d", now_heads_.size ()));
      // Find a previous note that ties to the current note. If it exists, 
      // remove it from the heads_to_tie vector and create the tie
      deque<Head_event_tuple>::iterator it;
      bool found = false;
      Stream_event *right_mus = inf.event_;
      for ( it = heads_to_tie_.begin() ; (!found) && (it < heads_to_tie_.end()); it++ )
        {
	  Audio_element_info et = (*it).head_;
	  Audio_note *th = dynamic_cast<Audio_note *> (et.elem_);
	  Stream_event *left_mus = et.event_;

	  if (th && right_mus && left_mus
	      && ly_is_equal (right_mus->get_property ("pitch"),
			      left_mus->get_property ("pitch")))
	    {
	      found = true;
	      Moment skip = now_mom() - (*it).moment_ - th->length_mom_;
	      an->tie_to (th, skip);
	      // this invalidates the iterator, we are leaving the loop anyway
	      heads_to_tie_.erase (it);
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
  // We might have dangling open ties like c~ d. Close them, unless we have
  // tieWaitForNote set...
  if (!to_boolean (get_property ("tieWaitForNote")))
    {
      heads_to_tie_.clear ();
    }

  if (event_)
    {
      for (vsize i = now_heads_.size (); i--;)
        heads_to_tie_.push_back (now_heads_[i]);
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
		"tieWaitForNote",

		/* write */
		"tieMelismaBusy"
		);
