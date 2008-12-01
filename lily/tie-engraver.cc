/*
  tie-engraver.cc -- implement Tie_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

#include "context.hh"
#include "international.hh"
#include "item.hh"
#include "note-head.hh"
#include "protected-scm.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "tie-column.hh"
#include "tie.hh"
#include "warn.hh"

#include "translator.icc"

/**
   Manufacture ties.  Acknowledge noteheads, and put them into a
   priority queue. If we have a TieEvent, connect the notes that finish
   just at this time, and note that start at this time.

   TODO: Remove the dependency on musical info. We should tie on the
   basis of position and duration-log of the heads (not of the events).
*/

struct Head_event_tuple
{
  Grob *head_;
  Moment end_moment_;
  SCM tie_definition_;
  Stream_event *tie_stream_event_;
  Stream_event *tie_event_;
  
  Head_event_tuple ()
  {
    head_ = 0;
    tie_definition_ = SCM_EOL;
    tie_event_ = 0;
    tie_stream_event_ = 0;
  }
};

class Tie_engraver : public Engraver
{
  Stream_event *event_;
  vector<Grob*> now_heads_;
  vector<Head_event_tuple> heads_to_tie_;
  vector<Grob*> ties_;

  Spanner *tie_column_;

protected:
  void stop_translation_timestep ();
  virtual void derived_mark () const;
  void start_translation_timestep ();
  DECLARE_ACKNOWLEDGER (note_head);
  DECLARE_TRANSLATOR_LISTENER (tie);
  void process_music ();
  void typeset_tie (Grob *);
public:
  TRANSLATOR_DECLARATIONS (Tie_engraver);
};

void
Tie_engraver::derived_mark () const
{
  Engraver::derived_mark ();
  for (vsize i = 0; i < heads_to_tie_.size (); i++)
    scm_gc_mark (heads_to_tie_[i].tie_definition_);
}

Tie_engraver::Tie_engraver ()
{
  event_ = 0;
  tie_column_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Tie_engraver, tie);
void
Tie_engraver::listen_tie (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (event_, ev);
}

void
Tie_engraver::process_music ()
{
  bool busy = event_;
  for (vsize i = 0; !busy && i < heads_to_tie_.size (); i++)
    busy |=  (heads_to_tie_[i].tie_event_
	      || heads_to_tie_[i].tie_stream_event_);

  if (busy)
    context ()->set_property ("tieMelismaBusy", SCM_BOOL_T);
}

void
Tie_engraver::acknowledge_note_head (Grob_info i)
{
  Grob *h = i.grob ();

  now_heads_.push_back (h);
  for (vsize i = heads_to_tie_.size (); i--;)
    {
      Grob *th = heads_to_tie_[i].head_;
      Stream_event *right_ev = unsmob_stream_event (h->get_property ("cause"));
      Stream_event *left_ev = unsmob_stream_event (th->get_property ("cause"));

      /*
	maybe should check positions too.
      */
      if (!right_ev || !left_ev)
	continue;
      
      if (ly_is_equal (right_ev->get_property ("pitch"),
		       left_ev->get_property ("pitch")))
	{
	  Grob *p = new Spanner (heads_to_tie_[i].tie_definition_);

	  SCM cause = heads_to_tie_[i].tie_event_
	    ? heads_to_tie_[i].tie_event_->self_scm ()
	    : heads_to_tie_[i].tie_stream_event_->self_scm ();
	  
	  announce_grob (p, cause);
	  Tie::set_head (p, LEFT, th);
	  Tie::set_head (p, RIGHT, h);


	  if (is_direction (unsmob_stream_event (cause)->get_property ("direction")))
	    {
	      Direction d = to_dir (unsmob_stream_event (cause)->get_property ("direction"));
	      p->set_property ("direction", scm_from_int (d)); 
	    }
	  
	  ties_.push_back (p);
	  heads_to_tie_.erase (heads_to_tie_.begin () + i);
	}
    }

  if (ties_.size () && ! tie_column_)
    tie_column_ = make_spanner ("TieColumn", ties_[0]->self_scm ());

  if (tie_column_)
    for (vsize i = ties_.size (); i--;)
      Tie_column::add_tie (tie_column_, ties_[i]);
}

void
Tie_engraver::start_translation_timestep ()
{
  context ()->set_property ("tieMelismaBusy",
			    ly_bool2scm (heads_to_tie_.size ()));

  if (heads_to_tie_.size () && !to_boolean (get_property ("tieWaitForNote")))
    {
      Moment now = now_mom ();
      for (vsize i = heads_to_tie_.size ();  i--; )
	{
	  if (now > heads_to_tie_[i].end_moment_)
	    heads_to_tie_.erase (heads_to_tie_.begin () + i);
	}
    }
}

void
Tie_engraver::stop_translation_timestep ()
{
  bool wait = to_boolean (get_property ("tieWaitForNote"));
  if (ties_.size ())
    {
      if (!wait)
	heads_to_tie_.clear ();

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
	= unsmob_stream_event (head->get_property ("cause"));

      if (!left_ev)
	{
	  // may happen for ambituses
	  continue;
	}
	    
      
      SCM left_articulations = left_ev->get_property ("articulations");

      Stream_event *tie_event = 0;
      Stream_event *tie_stream_event = event_;
      for (SCM s = left_articulations;
	   !tie_event && !tie_stream_event && scm_is_pair (s);
	   s = scm_cdr (s))
	{
	  Stream_event *ev = unsmob_stream_event (scm_car (s));
	  if (!ev)
	    continue;
	  
	  if (ev->in_event_class ("tie-event"))
	    tie_event = ev;
	}
	  
      if (left_ev && (tie_event || tie_stream_event))
	{
	  Head_event_tuple event_tup;

	  SCM start_definition
	    = updated_grob_properties (context (), ly_symbol2scm ("Tie"));

	  event_tup.head_ = head;
	  event_tup.tie_definition_ = start_definition;
	  event_tup.tie_event_ = tie_event;
	  event_tup.tie_stream_event_ = tie_stream_event;

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
    heads_to_tie_.clear ();

  // hmmm, how to do with copy () ?
  for (vsize i = 0; i < new_heads_to_tie.size (); i++)
    heads_to_tie_.push_back (new_heads_to_tie[i]);
  
  event_ = 0;
  now_heads_.clear ();
}

void
Tie_engraver::typeset_tie (Grob *her)
{
  if (! (Tie::head (her, LEFT) && Tie::head (her, RIGHT)))
    warning (_ ("lonely tie"));

  Direction d = LEFT;
  Drul_array<Grob *> new_head_drul;
  new_head_drul[LEFT] = Tie::head (her, LEFT);
  new_head_drul[RIGHT] = Tie::head (her, RIGHT);
  do
    {
      if (!Tie::head (her, d))
	new_head_drul[d] = Tie::head (her, (Direction) - d);
    }
  while (flip (&d) != LEFT);

  Spanner *sp = dynamic_cast<Spanner*> (her);
  sp->set_bound (LEFT, new_head_drul[LEFT]);
  sp->set_bound (RIGHT, new_head_drul[RIGHT]);
}

ADD_ACKNOWLEDGER (Tie_engraver, note_head);
ADD_TRANSLATOR (Tie_engraver,
		/* doc */
		"Generate ties between note heads of equal pitch.",

		/* create */
		"Tie "
		"TieColumn ",

		/* read */
		"tieWaitForNote ",

		/* write */
		"tieMelismaBusy "
		);
