/*
  porrectus-engraver.cc -- implement Porrectus_engraver

  Copyright (C) 2001 Juergen Reuter

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
#include "musical-request.hh"
#include "command-request.hh"
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
  virtual bool try_music (Music *req_l);
  virtual void process_music ();
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);

private:
  PQueue<Grob_pitch_tuple> past_notes_pq_;
  Porrectus_req *porrectus_req_l_;
  Array<Grob_pitch_tuple> left_heads_;
  Array<Grob_pitch_tuple> right_heads_;
  Link_array<Grob> porrectus_p_arr_;
};

Porrectus_engraver::Porrectus_engraver ()
{
  porrectus_req_l_ = 0;
}

bool
Porrectus_engraver::try_music (Music *m)
{
  if (Porrectus_req *req_l_ = dynamic_cast <Porrectus_req *> (m))
    {
      porrectus_req_l_ = req_l_;
      return true;
    }
  else
    return false;
}

void
Porrectus_engraver::process_music ()
{
  if (porrectus_req_l_)
    {
      top_engraver ()->forbid_breaks ();
    }
}

void
Porrectus_engraver::acknowledge_grob (Grob_info info_l_)
{
  if (Rhythmic_head::has_interface (info_l_.grob_l_))
    {
      Note_req *note_req_l_ = dynamic_cast <Note_req *> (info_l_.music_cause ());
      if (!note_req_l_)
	return;
      right_heads_.push (Grob_pitch_tuple (info_l_.grob_l_, note_req_l_,
					      now_mom () +
					      note_req_l_->length_mom ()));
    }
}

void
Porrectus_engraver::create_grobs ()
{
  if (porrectus_req_l_)
    {
      left_heads_.sort (Grob_pitch_tuple::pitch_compare);
      right_heads_.sort (Grob_pitch_tuple::pitch_compare);
      int i = left_heads_.size () - 1;
      int j = right_heads_.size () - 1;

      while ((i >= 0) && (j >= 0))
	{
	  Item *left_head = dynamic_cast<Item*> (left_heads_[i].head_l_);
	  Item *right_head = dynamic_cast<Item*> (right_heads_[j].head_l_);
	  left_head->set_grob_property("transparent", gh_bool2scm(true));
	  right_head->set_grob_property("transparent", gh_bool2scm(true));

	  Grob *porrectus_p_ = new Item (get_property ("Porrectus"));
	  Porrectus::set_left_head(porrectus_p_, left_head);
	  Porrectus::set_right_head(porrectus_p_, right_head);
	  porrectus_p_arr_.push (porrectus_p_);
	  announce_grob (porrectus_p_, porrectus_req_l_);

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

  for (int i = 0; i < porrectus_p_arr_.size (); i++)
    {
      typeset_grob (porrectus_p_arr_[i]);
    }
  porrectus_p_arr_.clear ();
}

void
Porrectus_engraver::start_translation_timestep ()
{
  porrectus_req_l_ = 0;
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
/* acks  */       "rhythmic-head-interface",
/* reads */       "",
/* write */       "");
