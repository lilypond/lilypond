/*
  tie-reg.cc -- implement Ties_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "ties-engraver.hh"
#include "tie.hh"
#include "note-head.hh"
#include "musical-request.hh"
#include "music-list.hh"

Ties_engraver::Ties_engraver()
{
  req_l_ = end_req_l_ =0;
  processed_ack_pass_i_ = 0;
  
}

void
Ties_engraver::do_post_move_processing()
{
  processed_ack_pass_i_ =0;
}

bool
Ties_engraver::do_try_request (Request*req)
{
  if (! req->access_Musical_req ())
      return false;

  Tie_req * r=  req->access_Musical_req ()->access_Tie_req ();
  if (!r)
    return false;
  
  req_l_ = r;
  return true;
}


void
Ties_engraver::acknowledge_element (Score_element_info i)
{
  if (!req_l_ && ! end_req_l_)
    return;
  if (i.elem_l_->is_type_b (Note_head::static_name ()))
    {
      Note_head * h = (Note_head*)i.elem_l_->access_Item ();
      Melodic_req *m = i.req_l_->access_Musical_req ()->access_Melodic_req ();
      
      head_mel_tuple_arr_.push (Head_melodic_tuple (h, m));
    }
}

void
Ties_engraver::process_acknowledged ()
{
  if (!head_mel_tuple_arr_.size () || processed_ack_pass_i_ ++)
    return;

  head_mel_tuple_arr_.sort (Head_melodic_tuple::compare);
  if (req_l_ && !tie_p_arr_.size ())
    {
      for (int i=0; i < head_mel_tuple_arr_.size (); i++)
	{
	  Tie*  p = new Tie;
	  p->set_head (LEFT,head_mel_tuple_arr_[i].head_l_);
	  //	  announce_element (Score_element_info (p, req_l_));
	  tie_p_arr_.push (p);
	}
    }

  if (end_req_l_)
    {
      for (int i=0; i < end_tie_p_arr_.size (); i++)
	{
	  int j = i;
	  if (j >= head_mel_tuple_arr_.size ())
	    {
	      left_head_mel_tuple_arr_[i].mel_l_->warning (_( "Can't find a note head at the right to attach Tie"));
	      j = head_mel_tuple_arr_.size () -1;
	    }
	  
	  Tie*p=end_tie_p_arr_[i];
	  p->set_head (RIGHT, head_mel_tuple_arr_[j].head_l_);
	  if (!Melodic_req::compare (*head_mel_tuple_arr_[j].mel_l_,
				     *left_head_mel_tuple_arr_[j].mel_l_))
	    p->same_pitch_b_ = true;
	  announce_element ( Score_element_info (p, end_req_l_));
	}
    }
}


void
Ties_engraver::do_pre_move_processing()
{
  if (!head_mel_tuple_arr_.size ())
    return;


  for (int i =0; i < end_tie_p_arr_.size (); i++)
    {
      Scalar tie_dir (get_property ("tieYDirection"));
      Scalar y_dir (get_property ("ydirection"));      
      Direction dir = CENTER;
      if (tie_dir.length_i () && tie_dir.isnum_b ())
	dir = (Direction) sign (int (tie_dir));
      else if (y_dir.length_i () && y_dir.isnum_b ())
	dir = (Direction) sign (int (y_dir));
      
      end_tie_p_arr_[i]->dir_ = dir;
      typeset_element (end_tie_p_arr_[i]);
    }

  end_tie_p_arr_ = tie_p_arr_;
  left_head_mel_tuple_arr_ = head_mel_tuple_arr_;
  end_req_l_ = req_l_;
  
  req_l_ =0;
  head_mel_tuple_arr_.clear ();
  tie_p_arr_.clear ();
}

void
Ties_engraver::do_removal_processing ()
{
}

void
Ties_engraver::do_process_requests ()
{}


IMPLEMENT_IS_TYPE_B1(Ties_engraver,Engraver);
ADD_THIS_TRANSLATOR(Ties_engraver);


Head_melodic_tuple::Head_melodic_tuple ()
{
  head_l_ =0;
  mel_l_ =0;
}

Head_melodic_tuple::Head_melodic_tuple (Note_head *h, Melodic_req*m)
{
  head_l_ = h;
  mel_l_ = m;
}

int
Head_melodic_tuple::compare (Head_melodic_tuple const&h1,
			     Head_melodic_tuple const &h2)
{
  return Melodic_req::compare (*h1.mel_l_, *h2.mel_l_);
}
