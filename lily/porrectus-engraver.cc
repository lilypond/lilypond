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
 * TODO: Introduce "\~" as alternative syntax for "\porrectus"?
 *
 * TODO: Hufnagel support.
 *
 * TODO: Fine-tuning of porrectus shape.  In particular, the mensural
 * non-solid shape could either be slightly bigger in height, or the
 * extrem points could be slightly vertically shifted apart.
 *
 * TODO: For white mensural (i.e. #'style=#'mensural, #'solid=##f)
 * porrectus grobs, it is possible to automatically determine all
 * porrectus specific properties (add-stem, stem-direction) solely
 * from the duration of the contributing notes and time-signature.
 * Introduce a boolean grob property called auto-config, so that, if
 * turned on, lily automatically sets the remaining properties
 * properly.
 *
 * TODO: The following issues are not (and should not be) handled by
 * this engraver: (1) accidentals placement, (2) avoiding line
 * breaking inbetween porrectus, (3) spacing.  For example, currently
 * only the accidental for the second note (cp. the above FIXME) is
 * printed.  These issues should be resolved by some sort of ligature
 * context that encloses use of this engraver, using syntax like:
 * \ligature { e \porrectus c }.
 *
 * TODO: Do not allow a series of adjacent porrectus requests, as in:
 * e \porrectus d \porrectus c.
 */

#include "staff-symbol-referencer.hh"
#include "porrectus.hh"
#include "musical-request.hh"
#include "command-request.hh"
#include "rhythmic-head.hh"
#include "item.hh"
#include "engraver.hh"
#include "pqueue.hh"

// TODO: PHead_melodic_tuple is duplicated code from tie-engraver.cc.
// Maybe put this into public class?
struct PHead_melodic_tuple {
  Melodic_req *req_l_;
  Grob *head_l_;
  Moment end_;
  PHead_melodic_tuple ();
  PHead_melodic_tuple (Grob*, Melodic_req*, Moment);
  static int pitch_compare (PHead_melodic_tuple const &,
			    PHead_melodic_tuple const &);
  static int time_compare (PHead_melodic_tuple const &,
			   PHead_melodic_tuple const &);  
};

inline int compare (PHead_melodic_tuple const &a, PHead_melodic_tuple const &b)
{
  return PHead_melodic_tuple::time_compare (a,b);
}

class Porrectus_engraver : public Engraver {
public:
  Porrectus_engraver ();
  VIRTUAL_COPY_CONS (Translator);
  
protected:
  virtual bool try_music (Music *req_l);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);

private:
  PQueue<PHead_melodic_tuple> past_notes_pq_;
  Porrectus_req *porrectus_req_l_;
  Array<PHead_melodic_tuple> left_heads_;
  Array<PHead_melodic_tuple> right_heads_;
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
Porrectus_engraver::acknowledge_grob (Grob_info info_l_)
{
  if (Rhythmic_head::has_interface (info_l_.elem_l_))
    {
      Note_req *note_req_l_ = dynamic_cast <Note_req *> (info_l_.req_l_);
      if (!note_req_l_)
	return;
      left_heads_.push (PHead_melodic_tuple (info_l_.elem_l_, note_req_l_,
					     now_mom () +
					     note_req_l_->length_mom ()));
    }
}

void
Porrectus_engraver::create_grobs ()
{
  if (porrectus_req_l_)
    {
      left_heads_.sort (PHead_melodic_tuple::pitch_compare);
      right_heads_.sort (PHead_melodic_tuple::pitch_compare);

      SCM head_list = SCM_EOL;
      
      int i = left_heads_.size () - 1;
      int j = right_heads_.size () - 1;

      while ((i >= 0) && (j >= 0))
	{
	  head_list =
	    gh_cons (gh_cons (right_heads_[j].head_l_->self_scm (),
			      left_heads_[i].head_l_->self_scm ()),
		     head_list);

	  past_notes_pq_. insert (left_heads_[i]);
	  left_heads_.del (i);
	  right_heads_.del (j);
	  i--;
	  j--;
	}

      for (SCM s = head_list; gh_pair_p (s); s = gh_cdr (s))
	{
	  SCM caar = gh_caar (s);
	  SCM cdar = gh_cdar (s);

	  Item *left_head = dynamic_cast<Item*> (unsmob_grob (caar));
	  Item *right_head = dynamic_cast<Item*> (unsmob_grob (cdar));
	  left_head->set_grob_property("transparent", gh_bool2scm(true));
	  right_head->set_grob_property("transparent", gh_bool2scm(true));

	  Grob *porrectus_p_ = new Item (get_property ("Porrectus"));
	  Porrectus::set_left_head(porrectus_p_, caar);
	  Porrectus::set_right_head(porrectus_p_, cdar);
	  porrectus_p_arr_.push (porrectus_p_);
	  announce_grob (porrectus_p_, 0);
	}
    }
}

void
Porrectus_engraver::stop_translation_timestep ()
{
  for (int i = 0; i < left_heads_.size (); i++)
    {
      past_notes_pq_.insert (left_heads_[i]);
    }
  left_heads_.clear ();

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

  right_heads_.clear ();
  while (past_notes_pq_.size () &&
	 (past_notes_pq_.front ().end_ == now))
    right_heads_.push (past_notes_pq_.get ());
}

ADD_THIS_TRANSLATOR (Porrectus_engraver);

// TODO: PHead_melodic_tuple is duplicated code from tie-engraver.cc.
// Maybe put this into public class?

PHead_melodic_tuple::PHead_melodic_tuple ()
{
  head_l_ = 0;
  req_l_ = 0;
  end_ = 0;
}

PHead_melodic_tuple::PHead_melodic_tuple (Grob *h, Melodic_req*m, Moment mom)
{
  head_l_ = h;
  req_l_ = m;
  end_ = mom;
}

/*
  signed compare, should use pitch<? 
 */
int
PHead_melodic_tuple::pitch_compare (PHead_melodic_tuple const&h1,
				    PHead_melodic_tuple const &h2)
{
  SCM p1 = h1.req_l_->get_mus_property ("pitch");
  SCM p2 = h2.req_l_->get_mus_property ("pitch");
  
  int result = Pitch::compare (*unsmob_pitch (p1),
			       *unsmob_pitch (p2));
  return result;
}

int
PHead_melodic_tuple::time_compare (PHead_melodic_tuple const&h1,
				   PHead_melodic_tuple const &h2)
{
  int result = Moment::compare(h1.end_,  h2.end_);
  return result;
}
