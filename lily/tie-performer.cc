/*   
  tie-performer.cc --  implement 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "tie-performer.hh"
#include "command-request.hh"
#include "audio-item.hh"
#include "musical-request.hh"


ADD_THIS_TRANSLATOR (Tie_performer);

Tie_performer::Tie_performer()
{
  req_l_ = 0;
}

bool
Tie_performer::do_try_music (Music *m)
{
  if (Tie_req * c = dynamic_cast<Tie_req*> (m))
    {
      req_l_ = c;
      return true;
    }
  return false;
}

void
Tie_performer::acknowledge_element (Audio_element_info i)
{
  if (Audio_note *nh = dynamic_cast<Audio_note *> (i.elem_l_))
    {
      Note_req * m = dynamic_cast<Note_req* > (i.req_l_);
      if (!m)
	return;
      now_notes_.push (CNote_melodic_tuple (nh, m, now_mom()+ m->length_mom ()));
    }
}

void
Tie_performer::do_process_requests ()
{
  if (req_l_)
    {
      Moment now = now_mom ();
      Link_array<Audio_note> nharr;
      
      stopped_notes_.clear ();
      while (past_notes_pq_.size ()
	     && past_notes_pq_.front ().end_ == now)
	stopped_notes_.push (past_notes_pq_.get ());
    }
}

void
Tie_performer::process_acknowledged ()
{
  if (req_l_)
    {
      now_notes_.sort (CNote_melodic_tuple::pitch_compare);
      stopped_notes_.sort(CNote_melodic_tuple::pitch_compare);
      int i=0;
      int j=0;
      int tie_count=0;
      while  ( i < now_notes_.size () && j < stopped_notes_.size ())
	{
	  int comp
	    = Musical_pitch::compare (now_notes_[i].req_l_->pitch_ ,
				      stopped_notes_[j].req_l_->pitch_);

	  if (comp)
	    {
	      (comp < 0) ? i ++ : j++;
	      continue;
	    }
	  else
	    {
	      tie_count ++;

	      /* don't go around recreating ties that were already
		 made. Not infallible. Due to reordering in sort (),
		 we will make the wrong ties when notenotes are
		 added.  */
	      if (tie_count > tie_p_arr_.size ())
		{
		  Audio_tie * p = new Audio_tie;
		  p->set_note (LEFT, stopped_notes_[j].note_l_);
		  p->set_note (RIGHT, now_notes_[i].note_l_);
		  tie_p_arr_.push (p);
		      announce_element (Audio_element_info (p, req_l_));
		}
	      i++;
	      j++;
	      
	    }
	}
      
      if (!tie_p_arr_.size ())
	{
	  req_l_->warning (_("No ties were created!"));
	}
      
    }
}

void
Tie_performer::do_pre_move_processing ()
{
  for (int i=0; i < now_notes_.size (); i++)
    {
      past_notes_pq_.insert (now_notes_[i]);
    }
  now_notes_.clear ();

  for (int i=0; i<  tie_p_arr_.size (); i++)
   {
     //play_element (tie_p_arr_[i]);
     /*
       urg.
       doesn't work for c ~ c ~ c
      */
     tie_p_arr_[i]->note_l_drul_[LEFT]->length_mom_ +=
       tie_p_arr_[i]->note_l_drul_[RIGHT]->length_mom_;
     tie_p_arr_[i]->note_l_drul_[RIGHT]->length_mom_ = 0;
    }
  tie_p_arr_.clear ();
}

void
Tie_performer::do_post_move_processing ()
{
  req_l_ =0;
  Moment now = now_mom ();
  while (past_notes_pq_.size () && past_notes_pq_.front ().end_ < now)
    past_notes_pq_.delmin ();
}


CNote_melodic_tuple::CNote_melodic_tuple ()
{
  note_l_ =0;
  req_l_ =0;
  end_ = 0;
}

CNote_melodic_tuple::CNote_melodic_tuple (Audio_note *h, Melodic_req*m, Moment mom)
{
  note_l_ = h;
  req_l_ = m;
  end_ = mom;
}

int
CNote_melodic_tuple::pitch_compare (CNote_melodic_tuple const&h1,
				    CNote_melodic_tuple const &h2)
{
  return Melodic_req::compare (*h1.req_l_, *h2.req_l_);
}

int
CNote_melodic_tuple::time_compare (CNote_melodic_tuple const&h1,
				   CNote_melodic_tuple const &h2)
{
  return (h1.end_ - h2.end_ ).sign ();
}
