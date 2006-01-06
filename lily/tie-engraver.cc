/*
  tie-engraver.cc -- implement Tie_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "tie.hh"
#include "context.hh"
#include "protected-scm.hh"
#include "spanner.hh"
#include "tie-column.hh"
#include "engraver.hh"
#include "item.hh"
#include "grob-pitch-tuple.hh"
#include "warn.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"

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
  SCM tie_definition_;
  Music *event_;
  Head_event_tuple ()
  {
  }
  Head_event_tuple (Grob *h, Music *m, SCM def)
  {
    head_ = h;
    event_ = m;
    tie_definition_ = def;
  }
};

class Tie_engraver : public Engraver
{
  Music *event_;
  Link_array<Grob> now_heads_;
  Array<Head_event_tuple> heads_to_tie_;
  Link_array<Grob> ties_;

  Spanner *tie_column_;

protected:
  void stop_translation_timestep ();
  virtual void derived_mark () const;
  void start_translation_timestep ();
  DECLARE_ACKNOWLEDGER (note_head);
  virtual bool try_music (Music *);
  void process_music ();
  void typeset_tie (Grob *);
public:
  TRANSLATOR_DECLARATIONS (Tie_engraver);
};

void
Tie_engraver::derived_mark () const
{
  Engraver::derived_mark ();
  for (int i = 0; i < heads_to_tie_.size (); i++)
    scm_gc_mark (heads_to_tie_[i].tie_definition_);
}

Tie_engraver::Tie_engraver ()
{
  event_ = 0;
  tie_column_ = 0;
}

bool
Tie_engraver::try_music (Music *mus)
{
  if (mus->is_mus_type ("tie-event"))
    event_ = mus;

  return true;
}

void
Tie_engraver::process_music ()
{
  if (event_)
    context ()->set_property ("tieMelismaBusy", SCM_BOOL_T);
}

void
Tie_engraver::acknowledge_note_head (Grob_info i)
{
  Grob *h = i.grob ();
  now_heads_.push (h);
  for (int i = heads_to_tie_.size (); i--;)
    {
      Grob *th = heads_to_tie_[i].head_;
      Music *right_mus = unsmob_music (h->get_property ("cause"));
      Music *left_mus = unsmob_music (th->get_property ("cause"));

      /*
	maybe should check positions too.
      */
      if (right_mus && left_mus
	  && ly_is_equal (right_mus->get_property ("pitch"),
			  left_mus->get_property ("pitch")))
	{
	  Grob *p = new Spanner (heads_to_tie_[i].tie_definition_,
				 context ()->get_grob_key ("Tie"));
	  announce_grob (p, heads_to_tie_[i].event_->self_scm ());
	  Tie::set_head (p, LEFT, th);
	  Tie::set_head (p, RIGHT, h);

	  ties_.push (p);
	  heads_to_tie_.del (i);
	}
    }

  if (ties_.size () && ! tie_column_)
    tie_column_ = make_spanner ("TieColumn", ties_[0]->self_scm ());

  if (tie_column_)
    for (int i = ties_.size (); i--;)
      Tie_column::add_tie (tie_column_, ties_[i]);
}

void
Tie_engraver::start_translation_timestep ()
{
  context ()->set_property ("tieMelismaBusy",
			    ly_bool2scm (heads_to_tie_.size ()));
}

void
Tie_engraver::stop_translation_timestep ()
{
  if (ties_.size ())
    {
      if (!to_boolean (get_property ("tieWaitForNote")))
	heads_to_tie_.clear ();

      for (int i = 0; i < ties_.size (); i++)
	typeset_tie (ties_[i]);

      ties_.clear ();
      tie_column_ = 0;
    }

  if (event_)
    {
      SCM start_definition
	= updated_grob_properties (context (), ly_symbol2scm ("Tie"));

      if (!to_boolean (get_property ("tieWaitForNote")))
	heads_to_tie_.clear ();

      for (int i = 0; i < now_heads_.size (); i++)
	{
	  heads_to_tie_.push (Head_event_tuple (now_heads_[i], event_,
						start_definition));
	}
    }

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

#include "translator.icc"

ADD_ACKNOWLEDGER (Tie_engraver, note_head);
ADD_TRANSLATOR (Tie_engraver,
		/* doc */ "Generate ties between noteheads of equal pitch.",
		/* create */
		"Tie "
		"TieColumn",

		/* accept */ "tie-event",
		/* read */ "tieWaitForNote",
		/* write */ "tieMelismaBusy");
