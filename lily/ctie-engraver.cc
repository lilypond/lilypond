/*   
  ctie-engraver.cc --  implement Command_tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "ctie-engraver.hh"
#include "command-request.hh"
#include "note-head.hh"
#include "musical-request.hh"

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
  
}

void
Command_tie_engraver::processed_acknowledged ()
{
}

void
Command_tie_engraver::do_pre_move_processing ()
{
  for (int i=0; i < now_heads_.size (); i++)
    {
      past_notes_pq_.insert (now_heads_[i]);
    }
}

void
Command_tie_engraver::do_post_move_processing ()
{
  Moment now = now_moment ();
  while (past_notes_pq_.front ().end_ < now)
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
