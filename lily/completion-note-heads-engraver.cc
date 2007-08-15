/*
  completion-note-heads-engraver.cc -- Completion_heads_engraver

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  vector<Grob*> ties_;

  vector<Item*> dots_;
  vector<Stream_event*> note_events_;
  vector<Stream_event*> scratch_note_events_;

  Moment note_end_mom_;
  bool is_first_;
  Rational left_to_do_;
  Rational do_nothing_until_;

  Moment next_barline_moment ();
  Duration find_nearest_duration (Rational length);

public:
  TRANSLATOR_DECLARATIONS (Completion_heads_engraver);

protected:
  virtual void initialize ();
  void start_translation_timestep ();
  void process_music ();
  void stop_translation_timestep ();
  DECLARE_TRANSLATOR_LISTENER (note);
};

void
Completion_heads_engraver::initialize ()
{
  is_first_ = false;
}

IMPLEMENT_TRANSLATOR_LISTENER (Completion_heads_engraver, note);
void
Completion_heads_engraver::listen_note (Stream_event *ev)
{
  note_events_.push_back (ev);
  
  is_first_ = true;
  Moment musiclen = get_event_length (ev);
  Moment now = now_mom ();

  if (now_mom ().grace_part_)
    {
      musiclen.grace_part_ = musiclen.main_part_;
      musiclen.main_part_ = Rational (0, 1);
    }

  note_end_mom_ = max (note_end_mom_, (now + musiclen));
  do_nothing_until_ = Rational (0, 0);
}

/*
  The duration _until_ the next barline.
*/
Moment
Completion_heads_engraver::next_barline_moment ()
{
  Moment *e = unsmob_moment (get_property ("measurePosition"));
  Moment *l = unsmob_moment (get_property ("measureLength"));
  if (!e || !l)
    {
      programming_error ("no timing props set?");
      return Moment (1, 1);
    }

  return (*l - *e);
}

Duration
Completion_heads_engraver::find_nearest_duration (Rational length)
{
  int log_limit = 6;

  Duration d (0, 0);

  /*
    this could surely be done more efficient. Left to the reader as an
    excercise.  */
  while (d.get_length () > length && d.duration_log () < log_limit)
    {
      if (d.dot_count ())
	{
	  d = Duration (d.duration_log (), d.dot_count ()- 1);
	  continue;
	}
      else
	d = Duration (d.duration_log () + 1, 2);
    }

  if (d.duration_log () >= log_limit)
    {
      // junk the dots.
      d = Duration (d.duration_log (), 0);

      // scale up.
      d = d.compressed (length / d.get_length ());
    }

  return d;
}

void
Completion_heads_engraver::process_music ()
{
  if (!is_first_ && !left_to_do_)
    return;

  is_first_ = false;

  Moment now = now_mom ();
  if (do_nothing_until_ > now.main_part_)
    return;

  Duration note_dur;
  Duration *orig = 0;
  if (left_to_do_)
    note_dur = find_nearest_duration (left_to_do_);
  else
    {
      orig = unsmob_duration (note_events_[0]->get_property ("duration"));
      note_dur = *orig;
    }
  Moment nb = next_barline_moment ();
  if (nb < note_dur.get_length ())
    {
      note_dur = find_nearest_duration (nb.main_part_);

      Moment next = now;
      next.main_part_ += note_dur.get_length ();

      get_global_context ()->add_moment_to_process (next);
      do_nothing_until_ = next.main_part_;
    }

  if (orig)
    left_to_do_ = orig->get_length ();

  if (orig && note_dur.get_length () != orig->get_length ())
    {
      if (!scratch_note_events_.size ())
	for (vsize i = 0; i < note_events_.size (); i++)
	  {
	    Stream_event *m = note_events_[i]->clone ();
	    scratch_note_events_.push_back (m);
	  }
    }
  else
    {
      for (vsize i = 0; i < note_events_.size (); i++)
	{
	  Stream_event *c =  note_events_[i]->clone ();
	  scratch_note_events_.push_back (c);
	}
    }
  
  for (vsize i = 0; left_to_do_ && i < note_events_.size (); i++)
    {
      Stream_event *event = note_events_[i];
      if (scratch_note_events_.size ())
	{
	  event = scratch_note_events_[i];
	  SCM pits = note_events_[i]->get_property ("pitch");
	  event->set_property ("pitch", pits);
	}

      event->set_property ("duration", note_dur.smobbed_copy ());

      Item *note = make_item ("NoteHead", event->self_scm ());
      note->set_property ("duration-log",
			  scm_from_int (note_dur.duration_log ()));

      int dots = note_dur.dot_count ();
      if (dots)
	{
	  Item *d = make_item ("Dots", SCM_EOL);
	  Rhythmic_head::set_dots (note, d);

	  d->set_property ("dot-count", scm_from_int (dots));

	  d->set_parent (note, Y_AXIS);
	  dots_.push_back (d);
	}

      Pitch *pit = unsmob_pitch (event->get_property ("pitch"));

      int pos = pit->steps ();
      SCM c0 = get_property ("middleCPosition");
      if (scm_is_number (c0))
	pos += scm_to_int (c0);

      note->set_property ("staff-position", scm_from_int (pos));
      notes_.push_back (note);
    }

  if (prev_notes_.size () == notes_.size ())
    {
      for (vsize i = 0; i < notes_.size (); i++)
	{
	  Grob *p = make_spanner ("Tie", SCM_EOL);
	  Tie::set_head (p, LEFT, prev_notes_[i]);
	  Tie::set_head (p, RIGHT, notes_[i]);

	  ties_.push_back (p);
	}
    }

  left_to_do_ -= note_dur.get_length ();

  /*
    don't do complicated arithmetic with grace notes.
  */
  if (orig
      && now_mom ().grace_part_)
    left_to_do_ = Rational (0, 0);
}

void
Completion_heads_engraver::stop_translation_timestep ()
{
  ties_.clear ();

  if (notes_.size ())
    prev_notes_ = notes_;
  notes_.clear ();

  dots_.clear ();

  for (vsize i = scratch_note_events_.size (); i--;)
    scratch_note_events_[i]->unprotect ();

  scratch_note_events_.clear ();
}

void
Completion_heads_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();
  if (note_end_mom_.main_part_ <= now.main_part_)
    {
      note_events_.clear ();
      prev_notes_.clear ();
    }
}

Completion_heads_engraver::Completion_heads_engraver ()
{
}

ADD_TRANSLATOR (Completion_heads_engraver,
		/* doc */ "This engraver replaces "
		"@code{Note_heads_engraver}. It plays some trickery to "
		"break long notes and automatically tie them into the next measure.",
		/* create */
		"NoteHead "
		"Dots "
		"Tie",
		/* read */
		"middleCPosition "
		"measurePosition "
		"measureLength",

		/* write */ "");
