/*
  porrectus-engraver.cc -- implement Porrectus_engraver

  Copyright (c) 2001--2002  Juergen Reuter

  written for the GNU LilyPond music typesetter
*/

/*
 * FIXME: Currently, when creating a porrectus item, it takes the
 * moment of the second note.  Actually, it should take the moment of
 * the first note.
 *
 * FIXME: Turn off typesetting of stems, flags, dots, etc.
 *
 * TODO: Hufnagel support.
 *
 * TODO: The following issues are currently not handled by this
 * engraver: (1) accidentals placement, (2) spacing.  For example,
 * currently only the accidental for the second note (cp. the above
 * FIXME) is printed.  These issues should be resolved by some sort of
 * ligature context that encloses use of this engraver, using syntax
 * like: \ligature { e \~ c }.
 *
 * TODO: Do not allow a series of adjacent porrectus requests, as in:
 * e \~ d \~ c.
 *
 * TODO: Junk duplicate (or rather triple) implementation of
 * create_ledger_line in porrectus.cc, custos.cc and note-head.cc.
 */

#include "staff-symbol-referencer.hh"
#include "porrectus.hh"
#include "request.hh"

#include "rhythmic-head.hh"
#include "item.hh"
#include "engraver.hh"
#include "score-engraver.hh"
#include "pqueue.hh"
#include "warn.hh"
#include "grob-pitch-tuple.hh"

class Porrectus_engraver : public Engraver {
public:
  TRANSLATOR_DECLARATIONS(Porrectus_engraver);
  
protected:
  virtual bool try_music (Music *req);
  virtual void process_music ();
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);

private:
  PQueue<Grob_pitch_tuple> past_notes_pq_;
  Music *porrectus_req_;
  Array<Grob_pitch_tuple> left_heads_;
  Array<Grob_pitch_tuple> right_heads_;
  Link_array<Grob> porrectuses_;
};

Porrectus_engraver::Porrectus_engraver ()
{
  porrectus_req_ = 0;
}

bool
Porrectus_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("porrectus-event"))
    {
      porrectus_req_ = m;
      return true;
    }
  else
    return false;
}

void
Porrectus_engraver::process_music ()
{
  if (porrectus_req_)
    {
      top_engraver ()->forbid_breaks ();
    }
}

void
Porrectus_engraver::acknowledge_grob (Grob_info info_)
{
  if (Rhythmic_head::has_interface (info_.grob_))
    {
      Music * m = info_.music_cause ();
      if (m->is_mus_type ("note-event"))
	right_heads_.push (Grob_pitch_tuple (info_.grob_, m,
					     now_mom () +
					     m->get_length ()));
    }
}

void
Porrectus_engraver::process_acknowledged_grobs ()
{
  if (porrectus_req_)
    {
      left_heads_.sort (Grob_pitch_tuple::pitch_compare);
      right_heads_.sort (Grob_pitch_tuple::pitch_compare);
      int i = left_heads_.size () - 1;
      int j = right_heads_.size () - 1;

      while ((i >= 0) && (j >= 0))
	{
	  Item *left_head = dynamic_cast<Item*> (left_heads_[i].head_);
	  Item *right_head = dynamic_cast<Item*> (right_heads_[j].head_);
	  left_head->set_grob_property("transparent", gh_bool2scm(true));
	  right_head->set_grob_property("transparent", gh_bool2scm(true));

	  Grob *porrectus_ = new Item (get_property ("Porrectus"));
	  Porrectus::set_left_head(porrectus_, left_head);
	  Porrectus::set_right_head(porrectus_, right_head);
	  porrectuses_.push (porrectus_);
	  announce_grob(porrectus_, porrectus_req_->self_scm());

	  past_notes_pq_. insert (right_heads_[i]);
	  left_heads_.del (i);
	  right_heads_.del (j);
	  i--;
	  j--;
	}
    }
}

void
Porrectus_engraver::stop_translation_timestep ()
{
  for (int i = 0; i < right_heads_.size (); i++)
    {
      past_notes_pq_.insert (right_heads_[i]);
    }
  right_heads_.clear ();

  for (int i = 0; i < porrectuses_.size (); i++)
    {
      typeset_grob (porrectuses_[i]);
    }
  porrectuses_.clear ();
}

void
Porrectus_engraver::start_translation_timestep ()
{
  porrectus_req_ = 0;
  Moment now = now_mom ();
  while (past_notes_pq_.size () && past_notes_pq_.front ().end_ < now)
    past_notes_pq_.delmin ();

  left_heads_.clear ();
  while (past_notes_pq_.size () &&
	 (past_notes_pq_.front ().end_ == now))
    left_heads_.push (past_notes_pq_.get ());
}



ENTER_DESCRIPTION(Porrectus_engraver,
/* descr */       "Join adjacent notes to a porrectus ligature.",
/* creats*/       "Porrectus",
/* accepts */     "porrectus-event",
/* acks  */      "rhythmic-head-interface",
/* reads */       "",
/* write */       "");
