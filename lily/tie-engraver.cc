/*   
  new-tie-engraver.cc --  implement Tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "event.hh"
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
class Tie_engraver : public Engraver
{
  Music *event_;
  Music *last_event_;
  Link_array<Grob> now_heads_;
  Link_array<Grob> heads_to_tie_;
  Link_array<Grob> ties_;
  SCM tie_start_definition_;
  
  Spanner * tie_column_;
  
protected:
  virtual void stop_translation_timestep ();
  virtual void derived_mark () const;
  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void process_music ();
  void typeset_tie (Grob*);
public:
  TRANSLATOR_DECLARATIONS (Tie_engraver);
};



Tie_engraver::Tie_engraver ()
{
  event_ = 0;
  last_event_  = 0;
  tie_column_ = 0;
  tie_start_definition_ = SCM_EOL;
}

void
Tie_engraver::derived_mark () const
{
  scm_gc_mark (tie_start_definition_);
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
Tie_engraver::process_music ()
{
  if (event_)
    {
      context ()->set_property ("tieMelismaBusy", SCM_BOOL_T);
    }
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
	  Music * right_mus = unsmob_music (h->get_property ("cause"));
	  Music * left_mus = unsmob_music (th->get_property ("cause"));

	  /*
	    maybe should check positions too.
	   */
	  if (right_mus && left_mus
	      && ly_c_equal_p (right_mus->get_property ("pitch"),
			     left_mus->get_property ("pitch")))
	    {
	      Grob * p = new Spanner  (tie_start_definition_, context()->get_grob_key ("Tie"));
	      announce_grob (p, last_event_->self_scm ());
	      Tie::set_interface (p); // cannot remove yet!
	  
	      Tie::set_head (p, LEFT, th);
	      Tie::set_head (p, RIGHT, h);
	  
	      ties_.push (p);
	    }
	}

      if (ties_.size () && ! tie_column_)
	{
	  tie_column_ = make_spanner ("TieColumn", SCM_EOL);
	  
	}

      if (tie_column_)
	for (int i = ties_.size (); i--;)
	  Tie_column::add_tie (tie_column_,ties_ [i]);
    }
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
      heads_to_tie_.clear ();
      for (int i=0; i<  ties_.size (); i++)
	{
	  typeset_tie (ties_[i]);
	}

      ties_.clear ();
      last_event_ = 0;
      tie_column_ =0;
    }
  
  if (event_)
    {
      tie_start_definition_ = updated_grob_properties (context (), ly_symbol2scm ("Tie"));
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

  index_set_cell (her->get_property ("head-pair"), LEFT, new_head_drul[LEFT]->self_scm ());
  index_set_cell (her->get_property ("head-pair"), RIGHT, new_head_drul[RIGHT]->self_scm ());

}


ENTER_DESCRIPTION (Tie_engraver,
/* descr */       "Generate ties between noteheads of equal pitch.",
/* creats*/       "Tie TieColumn",
/* accepts */     "tie-event",
/* acks  */      "rhythmic-head-interface",
/* reads */       "tieMelismaBusy",
/* write */       "");
