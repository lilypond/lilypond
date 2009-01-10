/*
  completion-note-heads-engraver.cc -- Completion_heads_engraver

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <cctype>
using namespace std;

#include "dot-column.hh"
#include "dots.hh"
#include "duration.hh"
#include "global-context.hh"
#include "item.hh"
#include "output-def.hh"
#include "pitch.hh"
#include "pqueue.hh"
#include "rhythmic-head.hh"
#include "score-engraver.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "tie.hh"
#include "warn.hh"

#include "translator.icc"

/*
  TODO: make matching rest engraver.
*/
struct Pending_tie
{
  Moment when_;
  Stream_event* tie_event_;
  Pending_tie() { tie_event_ = 0; }
};

int compare(Pending_tie const &a, Pending_tie const &b)
{
  return compare(a.when_, b.when_);
}


/*
  How does this work?

  When we catch the note, we predict the end of the note. We keep the
  events living until we reach the predicted end-time.

  Every time process_music () is called and there are note events, we
  figure out how long the note to typeset should be. It should be no
  longer than what's specified, than what is left to do and it should
  not cross barlines.

  We copy the events into scratch note events, to make sure that we get
  all durations exactly right.
*/

class Completion_heads_engraver : public Engraver
{
  vector<Item*> notes_;
  vector<Item*> prev_notes_;

  // Must remember notes for explicit ties.
  vector<Item*> tie_note_candidates_;
  vector<Stream_event*> tie_note_candidate_events_;
  vector<Grob*> ties_;
  PQueue<Pending_tie> pending_ties_;
  vector<Stream_event*> note_events_;

  Stream_event *current_tie_event_;
  Moment note_end_mom_;
  bool is_first_;
  Rational left_to_do_;
  Rational do_nothing_until_;

  Moment next_barline_moment ();
  Item *make_note_head (Stream_event*);

public:
  TRANSLATOR_DECLARATIONS (Completion_heads_engraver);

protected:
  virtual void initialize ();
  void make_tie (Grob *, Grob *);
  void start_translation_timestep ();
  void process_music ();
  void stop_translation_timestep ();
  DECLARE_TRANSLATOR_LISTENER (note);
  DECLARE_TRANSLATOR_LISTENER (tie);
};

void
Completion_heads_engraver::initialize ()
{
  is_first_ = false;
  current_tie_event_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Completion_heads_engraver, note);
void
Completion_heads_engraver::listen_note (Stream_event *ev)
{
  note_events_.push_back (ev);
  
  is_first_ = true;
  Moment now = now_mom ();
  Moment musiclen = get_event_length (ev, now);

  note_end_mom_ = max (note_end_mom_, (now + musiclen));
  do_nothing_until_ = Rational (0, 0);
}

IMPLEMENT_TRANSLATOR_LISTENER (Completion_heads_engraver, tie);
void
Completion_heads_engraver::listen_tie (Stream_event *ev)
{
  is_first_ = true;
  current_tie_event_ = ev;
}

/*
  The duration _until_ the next barline.
*/
Moment
Completion_heads_engraver::next_barline_moment ()
{
  Moment *e = unsmob_moment (get_property ("measurePosition"));
  Moment *l = unsmob_moment (get_property ("measureLength"));
  if (!e || !l || !to_boolean (get_property ("timing")))
    {
      return Moment (0, 0);
    }

  return (*l - *e);
}

Item*
Completion_heads_engraver::make_note_head (Stream_event *ev)
{
  Item *note = make_item ("NoteHead", ev->self_scm ());
  Pitch *pit = unsmob_pitch (ev->get_property ("pitch"));

  int pos = pit->steps ();
  SCM c0 = get_property ("middleCPosition");
  if (scm_is_number (c0))
    pos += scm_to_int (c0);

  note->set_property ("staff-position", scm_from_int (pos));

  return note;
}

void
Completion_heads_engraver::process_music ()
{
  if (!is_first_ && !left_to_do_)
    return;

  if (current_tie_event_)
    {
      Pending_tie pending;
      pending.when_ = note_end_mom_;
      pending.tie_event_ = current_tie_event_;
      pending_ties_.insert (pending);
    }
  
  is_first_ = false;

  Moment now = now_mom ();
  if (do_nothing_until_ > now.main_part_)
    return;

  Duration note_dur;
  Duration *orig = 0;
  if (left_to_do_)
    note_dur = Duration (left_to_do_, false);
  else
    {
      orig = unsmob_duration (note_events_[0]->get_property ("duration"));
      note_dur = *orig;
    }
  Moment nb = next_barline_moment ();
  if (nb.main_part_ && nb < note_dur.get_length ())
    {
      note_dur = Duration (nb.main_part_, false);

      do_nothing_until_ = now.main_part_ + note_dur.get_length ();
    }

  if (orig)
    left_to_do_ = orig->get_length ();

  for (vsize i = 0; left_to_do_ && i < note_events_.size (); i++)
    {
      bool need_clone = !orig || *orig != note_dur;
      Stream_event *event = note_events_[i];

      if (need_clone)
	event = event->clone ();

      SCM pits = note_events_[i]->get_property ("pitch");

      event->set_property ("pitch", pits);
      event->set_property ("duration", note_dur.smobbed_copy ());
      event->set_property ("length", Moment (note_dur.get_length ()).smobbed_copy ());
      event->set_property ("duration-log", scm_from_int (note_dur.duration_log ()));

      Item *note = make_note_head (event);
      if (need_clone)
	event->unprotect ();
      notes_.push_back (note);
    }
  
  if (pending_ties_.size ()
      && pending_ties_.front().when_ == now_mom())
    {
      for (vsize i = 0; i < tie_note_candidate_events_.size(); i++)
	for (vsize j = 0; j < note_events_.size(); j++)
	  {
	    Pitch *p =  unsmob_pitch (note_events_[j]->get_property ("pitch"));
	    Pitch *p_last
	      = unsmob_pitch (tie_note_candidate_events_[j]->get_property ("pitch"));
	    if (p && p_last && *p == *p_last)
	      make_tie (tie_note_candidates_[i], notes_[j]);
	  }
    }
      
  if (prev_notes_.size () == notes_.size ())
    {
      for (vsize i = 0; i < notes_.size (); i++)
	make_tie (prev_notes_[i], notes_[i]);
    }

  left_to_do_ -= note_dur.get_length ();
  if (left_to_do_)
    get_global_context ()->add_moment_to_process (now.main_part_ + note_dur.get_length());
  /*
    don't do complicated arithmetic with grace notes.
  */
  if (orig && now_mom ().grace_part_)
    left_to_do_ = Rational (0, 0);
}

void
Completion_heads_engraver::make_tie (Grob *left, Grob *right)
{
  Grob *p = make_spanner ("Tie", SCM_EOL);
  Tie::set_head (p, LEFT, left);
  Tie::set_head (p, RIGHT, right);
  ties_.push_back (p);
}
				     
void
Completion_heads_engraver::stop_translation_timestep ()
{
  ties_.clear ();

  if (notes_.size ())
    prev_notes_ = notes_;
  notes_.clear ();
}

void
Completion_heads_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();
  while (pending_ties_.size() && pending_ties_.front().when_ < now)
    {
      pending_ties_.delmin();
    }
  current_tie_event_ = 0;
  if (note_end_mom_.main_part_ <= now.main_part_)
    {
      tie_note_candidate_events_ = note_events_;
      tie_note_candidates_ = prev_notes_;

      note_events_.clear ();
      prev_notes_.clear ();
    }
  context ()->set_property ("completionBusy",
			    ly_bool2scm (note_events_.size ()));
}

Completion_heads_engraver::Completion_heads_engraver ()
{
}

ADD_TRANSLATOR (Completion_heads_engraver,
		/* doc */
		"This engraver replaces @code{Note_heads_engraver}.  It plays"
		" some trickery to break long notes and automatically tie them"
		" into the next measure.",

		/* create */
		"NoteHead "
		"Dots "
		"Tie ",

		/* read */
		"middleCPosition "
		"measurePosition "
		"measureLength ",

		/* write */
		"completionBusy "
		);
