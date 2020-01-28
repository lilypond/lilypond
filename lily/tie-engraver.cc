/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "engraver.hh"

#include "context.hh"
#include "international.hh"
#include "item.hh"
#include "note-head.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "tie-column.hh"
#include "tie.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

/**
   Manufacture ties.  Acknowledge note heads, and put them into a
   priority queue. If we have a TieEvent, connect the notes that finish
   just at this time, and note that start at this time.

   TODO: Remove the dependency on musical info. We should tie on the
   basis of position and duration-log of the heads (not of the events).
*/

struct Head_event_tuple
{
  Grob *head_;
  Moment end_moment_;
  Stream_event *tie_stream_event_;
  Stream_event *tie_event_;
  Spanner *tie_;
  // Indicate whether a tie from the same moment has been processed successfully
  // This is needed for tied chords, e.g. <c e g>~ g, because otherwise the c
  // and e will trigger a warning for an unterminated tie!
  bool tie_from_chord_created;

  Head_event_tuple ()
  {
    head_ = 0;
    tie_event_ = 0;
    tie_stream_event_ = 0;
    tie_from_chord_created = false;
    tie_ = 0;
  }
};

class Tie_engraver : public Engraver
{
  /*
    Whether tie event has been processed and can be deleted or should
    be kept for later portions of a split note.
  */
  bool event_processed_;
  Stream_event *event_;
  vector<Grob *> now_heads_;
  vector<Head_event_tuple> heads_to_tie_;
  vector<Spanner *> ties_;

  Spanner *tie_column_;
  bool tie_notehead (Grob *h, bool enharmonic);

protected:
  void process_acknowledged ();
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void acknowledge_note_head (Grob_info);
  void listen_tie (Stream_event *);
  void process_music ();
  void typeset_tie (Spanner *);
  void report_unterminated_tie (Head_event_tuple const &);
  bool has_autosplit_end (Stream_event *event);

public:
  TRANSLATOR_DECLARATIONS (Tie_engraver);
};

Tie_engraver::Tie_engraver (Context *c) : Engraver (c)
{
  event_ = 0;
  tie_column_ = 0;
  event_processed_ = false;
}

void
Tie_engraver::listen_tie (Stream_event *ev)
{
  if (!to_boolean (get_property ("skipTypesetting")))
    {
      ASSIGN_EVENT_ONCE (event_, ev);
    }
}

void
Tie_engraver::report_unterminated_tie (Head_event_tuple const &tie_start)
{
  // If tie_from_chord_created is set, we have another note at the same
  // moment that created a tie, so this is not necessarily an unterminated
  // tie. Happens e.g. for <c e g>~ g
  if (!tie_start.tie_from_chord_created)
    {
      tie_start.tie_->warning (_ ("unterminated tie"));
      tie_start.tie_->suicide ();
    }
}

/*
  Determines whether the end of an event was created by
  a split in Completion_heads_engraver or by user input.
*/
bool
Tie_engraver::has_autosplit_end (Stream_event *event)
{
  if (event)
    return to_boolean (event->get_property ("autosplit-end"));
  return false;
}

void
Tie_engraver::process_music ()
{
  bool busy = event_;
  for (vsize i = 0; !busy && i < heads_to_tie_.size (); i++)
    busy |= (heads_to_tie_[i].tie_event_ || heads_to_tie_[i].tie_stream_event_);

  if (busy)
    context ()->set_property ("tieMelismaBusy", SCM_BOOL_T);
}

bool
Tie_engraver::tie_notehead (Grob *h, bool enharmonic)
{
  bool found = false;

  for (vsize i = 0; i < heads_to_tie_.size (); i++)
    {
      Grob *th = heads_to_tie_[i].head_;
      Stream_event *right_ev = unsmob<Stream_event> (h->get_property ("cause"));
      Stream_event *left_ev = unsmob<Stream_event> (th->get_property ("cause"));

      /*
        maybe should check positions too.
      */
      if (!right_ev || !left_ev)
        continue;

      /*
        Make a tie only if pitches are equal or if event end was not generated
        by Completion_heads_engraver.
      */
      SCM p1 = left_ev->get_property ("pitch");
      SCM p2 = right_ev->get_property ("pitch");
      if ((enharmonic ? (unsmob<Pitch> (p1) && unsmob<Pitch> (p2)
                         && unsmob<Pitch> (p1)->tone_pitch ()
                                == unsmob<Pitch> (p2)->tone_pitch ())
                      : ly_is_equal (p1, p2))
          && (!Tie_engraver::has_autosplit_end (left_ev)))
        {
          Spanner *p = heads_to_tie_[i].tie_;
          Moment end = heads_to_tie_[i].end_moment_;

          Stream_event *cause = heads_to_tie_[i].tie_event_
                                    ? heads_to_tie_[i].tie_event_
                                    : heads_to_tie_[i].tie_stream_event_;

          announce_end_grob (p, cause->self_scm ());

          Tie::set_head (p, LEFT, th);
          Tie::set_head (p, RIGHT, h);

          if (is_direction (cause->get_property ("direction")))
            {
              Direction d = to_dir (cause->get_property ("direction"));
              p->set_property ("direction", scm_from_int (d));
            }

          ties_.push_back (p);
          heads_to_tie_.erase (heads_to_tie_.begin () + i);

          found = true;
          /*
            Prevent all other tied notes ending at the same moment (assume
            implicitly the notes have also started at the same moment!)
            from triggering an "unterminated tie" warning. Needed e.g. for
            <c e g>~ g
          */
          for (vsize j = heads_to_tie_.size (); j--;)
            {
              if (heads_to_tie_[j].end_moment_ == end)
                heads_to_tie_[j].tie_from_chord_created = true;
            }
          break;
        }
    }
  return found;
}

void
Tie_engraver::acknowledge_note_head (Grob_info i)
{
  Grob *h = i.grob ();

  now_heads_.push_back (h);

  if (!tie_notehead (h, false))
    tie_notehead (h, true);

  if (ties_.size () && !tie_column_)
    tie_column_ = make_spanner ("TieColumn", ties_[0]->self_scm ());

  if (tie_column_)
    for (vsize i = 0; i < ties_.size (); i++)
      Tie_column::add_tie (tie_column_, ties_[i]);
}

void
Tie_engraver::start_translation_timestep ()
{
  if (heads_to_tie_.size () && !to_boolean (get_property ("tieWaitForNote")))
    {
      Moment now = now_mom ();
      for (vsize i = heads_to_tie_.size (); i--;)
        {
          if (now > heads_to_tie_[i].end_moment_)
            {
              report_unterminated_tie (heads_to_tie_[i]);
              heads_to_tie_.erase (heads_to_tie_.begin () + i);
            }
        }
    }

  context ()->set_property ("tieMelismaBusy",
                            ly_bool2scm (heads_to_tie_.size ()));
}

void
Tie_engraver::process_acknowledged ()
{
  bool wait = to_boolean (get_property ("tieWaitForNote"));
  if (ties_.size ())
    {
      if (!wait)
        {
          vector<Head_event_tuple>::iterator it = heads_to_tie_.begin ();
          for (; it < heads_to_tie_.end (); it++)
            report_unterminated_tie (*it);
          heads_to_tie_.clear ();
        }

      for (vsize i = 0; i < ties_.size (); i++)
        typeset_tie (ties_[i]);

      ties_.clear ();
      tie_column_ = 0;
    }

  vector<Head_event_tuple> new_heads_to_tie;

  for (vsize i = 0; i < now_heads_.size (); i++)
    {
      Grob *head = now_heads_[i];
      Stream_event *left_ev
          = unsmob<Stream_event> (head->get_property ("cause"));

      if (!left_ev)
        {
          // may happen for ambitus
          continue;
        }

      // We only want real notes to cause ties, not e.g. pitched trills
      if (!left_ev->in_event_class ("note-event"))
        continue;

      SCM left_articulations = left_ev->get_property ("articulations");

      Stream_event *tie_event = 0;
      Stream_event *tie_stream_event = event_;
      for (SCM s = left_articulations;
           !tie_event && !tie_stream_event && scm_is_pair (s); s = scm_cdr (s))
        {
          Stream_event *ev = unsmob<Stream_event> (scm_car (s));
          if (!ev)
            continue;

          if (ev->in_event_class ("tie-event"))
            tie_event = ev;
        }

      if (left_ev && (tie_event || tie_stream_event)
          && (!Tie_engraver::has_autosplit_end (left_ev)))
        {
          event_processed_ = true;

          Head_event_tuple event_tup;

          event_tup.head_ = head;
          event_tup.tie_event_ = tie_event;
          event_tup.tie_stream_event_ = tie_stream_event;
          event_tup.tie_
              = make_spanner ("Tie", tie_event ? tie_event->self_scm ()
                                               : tie_stream_event->self_scm ());

          Moment end = now_mom ();
          if (end.grace_part_)
            {
              end.grace_part_ += get_event_length (left_ev).main_part_;
            }
          else
            {
              end += get_event_length (left_ev);
            }
          event_tup.end_moment_ = end;

          new_heads_to_tie.push_back (event_tup);
        }
    }

  if (!wait && new_heads_to_tie.size ())
    {
      vector<Head_event_tuple>::iterator it = heads_to_tie_.begin ();
      for (; it < heads_to_tie_.end (); it++)
        report_unterminated_tie (*it);
      heads_to_tie_.clear ();
    }

  // hmmm, how to do with copy () ?
  for (vsize i = 0; i < new_heads_to_tie.size (); i++)
    heads_to_tie_.push_back (new_heads_to_tie[i]);

  now_heads_.clear ();
}

void
Tie_engraver::stop_translation_timestep ()
{
  /*
    Discard event only if it has been processed with at least one
    appropriate note.
  */
  if (event_processed_)
    event_ = 0;

  event_processed_ = false;
}

void
Tie_engraver::typeset_tie (Spanner *her)
{
  Grob *left_head = Tie::head (her, LEFT);
  Grob *right_head = Tie::head (her, RIGHT);

  if (!left_head || !right_head)
    {
      warning (_ ("lonely tie"));
      if (!left_head)
        left_head = right_head;
      else
        right_head = left_head;
    }

  her->set_bound (LEFT, left_head);
  her->set_bound (RIGHT, right_head);
}

void
Tie_engraver::boot ()
{
  ADD_LISTENER (Tie_engraver, tie);
  ADD_ACKNOWLEDGER (Tie_engraver, note_head);
}

ADD_TRANSLATOR (Tie_engraver,
                /* doc */
                "Generate ties between note heads of equal pitch.",

                /* create */
                "Tie "
                "TieColumn ",

                /* read */
                "skipTypesetting "
                "tieWaitForNote ",

                /* write */
                "tieMelismaBusy ");
