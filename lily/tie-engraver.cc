/*   
  new-tie-engraver.cc --  implement Tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "event.hh"
#include "tie.hh"
#include "translator-group.hh"
#include "spanner.hh"
#include "tie-column.hh"
#include "engraver.hh"
#include "item.hh"
#include "grob-pitch-tuple.hh"
#include "warn.hh"
#include "note-head.hh"

/**
   Manufacture ties.  Acknowledge noteheads, and put them into a
   priority queue. If we have a TieEvent, connect the notes that finish
   just at this time, and note that start at this time.

   TODO: Remove the dependency on musical info. We should tie on the
   basis of position and duration-log of the heads (not of the events).

   TODO: support sparseTies.

   TODO: melismata will fuck up now:

   < { c8 ~ c8 }
     { c16 c c c  } >

   melisma is after the 2nd 8th note, but will now be signaled as
   lasting till the 3rd 16th.
*/
class Tie_engraver : public Engraver
{
  Music *event_;
  Music *last_event_;
  Link_array<Grob> now_heads_;
  Link_array<Grob> heads_to_tie_;
  Link_array<Grob> ties_;
  
  Spanner * tie_column_;
  
  
protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void process_acknowledged_grobs ();
  void typeset_tie (Grob*);
public:
  TRANSLATOR_DECLARATIONS(Tie_engraver);
};



Tie_engraver::Tie_engraver ()
{
  event_ = 0;
  last_event_  = 0;
  tie_column_ = 0;
}


bool
Tie_engraver::try_music (Music *mus)
{
  if (mus->is_mus_type ("tie-event"))
    {
      event_ = mus;
    }
  
  return true;
}

void
Tie_engraver::acknowledge_grob (Grob_info i)
{
  if (Note_head::has_interface (i.grob_))
    {
      Grob * h  = i.grob_;
      now_heads_.push (h);
      for  (int i = heads_to_tie_.size (); i--;)
	{
	  Grob *th =  heads_to_tie_[i];
	  Music * right_mus = unsmob_music (h->get_grob_property ("cause"));
	  Music * left_mus = unsmob_music (th->get_grob_property ("cause"));

	  /*
	    maybe should check positions too.
          */
	  if (right_mus && left_mus
	      && gh_equal_p (right_mus->get_mus_property ("pitch"),
                            left_mus->get_mus_property ("pitch")))
	    {
	      Grob * p = new Spanner (get_property ("Tie"));
	      Tie::set_interface (p); // cannot remove yet!
	  
	      Tie::set_head (p, LEFT, th);
	      Tie::set_head (p, RIGHT, h);
	  
	      ties_.push (p);
	      announce_grob(p, last_event_->self_scm());
	    }
	}
    }
}

void
Tie_engraver::process_acknowledged_grobs ()
{
  if (ties_.size () > 1 && !tie_column_)
    {
      tie_column_ = new Spanner (get_property ("TieColumn"));
      
      for (int i = ties_.size (); i--;)
	Tie_column::add_tie (tie_column_,ties_ [i]);

      announce_grob(tie_column_, SCM_EOL);
    }
}

void
Tie_engraver::start_translation_timestep ()
{
  if (to_boolean (get_property ("automaticMelismata")))
      daddy_trans_->set_property ("tieMelismaBusy",
				  gh_bool2scm (heads_to_tie_.size ()));
      
}

void
Tie_engraver::stop_translation_timestep ()
{
  if (ties_.size ())
    {
      heads_to_tie_.clear ();
      for (int i=0; i<  ties_.size (); i++)
	{
	  typeset_tie (ties_[i]);
	}

      ties_.clear();
      last_event_ = 0;
      if (tie_column_)
	{
	  typeset_grob (tie_column_);
	  tie_column_ =0;
	}
    }
  
  if (event_)
    {
      heads_to_tie_ = now_heads_;
      last_event_ = event_;
    }
  event_ = 0;
  now_heads_.clear ();
}

void
Tie_engraver::typeset_tie (Grob *her)
{
  if (! (Tie::head (her,LEFT) && Tie::head (her,RIGHT)))
    warning (_ ("lonely tie"));

  Direction d = LEFT;
  Drul_array<Grob *> new_head_drul;
  new_head_drul[LEFT] = Tie::head (her,LEFT);
  new_head_drul[RIGHT] = Tie::head (her,RIGHT);  
  do {
    if (!Tie::head (her,d))
      new_head_drul[d] = Tie::head (her, (Direction)-d);
  } while (flip (&d) != LEFT);

  index_set_cell (her->get_grob_property ("heads"), LEFT, new_head_drul[LEFT]->self_scm ());
  index_set_cell (her->get_grob_property ("heads"), RIGHT, new_head_drul[RIGHT]->self_scm ());

  typeset_grob (her);
}


ENTER_DESCRIPTION(Tie_engraver,
/* descr */       "Generate ties between noteheads of equal pitch.",
/* creats*/       "Tie TieColumn",
/* accepts */     "tie-event",
/* acks  */      "rhythmic-head-interface",
/* reads */       "tieMelismaBusy",
/* write */       "");
