/*   
  ctie-engraver.cc --  implement Command_tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "ctie-engraver.hh"
#include "command-request.hh"
#include "note-head.hh"
#include "musical-request.hh"
#include "tie.hh"

Command_tie_engraver::Command_tie_engraver()
{
  req_l_ = 0;
}


bool
Command_tie_engraver::do_try_music (Music *m)
{
  if (Command_tie_req * c = dynamic_cast<Command_tie_req*> (m))
    {
      req_l_ = c;
      return true;
    }
  return false;
}

void
Command_tie_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_head *nh = dynamic_cast<Note_head *> (i.elem_l_))
    {
      Note_req * m = dynamic_cast<Note_req* > (i.req_l_);
      now_heads_.push (CHead_melodic_tuple (nh, m, now_moment()+ m->duration ()));
    }
}

void
Command_tie_engraver::do_process_requests ()
{
  if (req_l_)
    {
      Moment now = now_moment ();
      Link_array<Note_head> nharr;
      
      stopped_heads_.clear ();
      while (past_notes_pq_.size ()
	     && past_notes_pq_.front ().end_ == now)
	stopped_heads_.push (past_notes_pq_.get ());

    }
}

void
Command_tie_engraver::process_acknowledged ()
{
  if (req_l_)
    {
      if (now_heads_.size () != stopped_heads_.size ())
	{
	  req_l_->warning ("Unequal number of note heads for tie");
	}
      int sz = now_heads_.size () <? stopped_heads_.size ();

      // hmm. Should do something more sensible.
      // because, we assume no more noteheads come along after the 1st pass.
      if (sz > tie_p_arr_.size ())
	{
	  now_heads_.sort (CHead_melodic_tuple::pitch_compare);
	  stopped_heads_.sort(CHead_melodic_tuple::pitch_compare);

	  for (int i=0; i < sz; i++)
	    {
	      Tie * p = new Tie;
	      p->set_head (LEFT, stopped_heads_[i].head_l_);
	      p->set_head (RIGHT, now_heads_[i].head_l_);
	      tie_p_arr_.push (p);
	      announce_element (Score_element_info (p, req_l_));
	    }
	}
    }
}

void
Command_tie_engraver::do_pre_move_processing ()
{
  for (int i=0; i < now_heads_.size (); i++)
    {
      past_notes_pq_.insert (now_heads_[i]);
    }
  now_heads_.clear( );
  
  for (int i=0; i<  tie_p_arr_.size (); i++)
    {
      typeset_element (tie_p_arr_[i]);
    }
  tie_p_arr_.clear ();
}

void
Command_tie_engraver::do_post_move_processing ()
{
  req_l_ =0;
  Moment now = now_moment ();
  while (past_notes_pq_.size () && past_notes_pq_.front ().end_ < now)
    past_notes_pq_.delmin ();
}



ADD_THIS_TRANSLATOR(Command_tie_engraver);


CHead_melodic_tuple::CHead_melodic_tuple ()
{
  head_l_ =0;
  mel_l_ =0;
  end_ = 0;
}

CHead_melodic_tuple::CHead_melodic_tuple (Note_head *h, Melodic_req*m, Moment mom)
{
  head_l_ = h;
  mel_l_ = m;
  end_ = mom;
}

int
CHead_melodic_tuple::pitch_compare (CHead_melodic_tuple const&h1,
			     CHead_melodic_tuple const &h2)
{
  return Melodic_req::compare (*h1.mel_l_, *h2.mel_l_);
}

int
CHead_melodic_tuple::time_compare (CHead_melodic_tuple const&h1,
			     CHead_melodic_tuple const &h2)
{
  return (h1.end_ - h2.end_ ).sign ();
}
