/*
  key-reg.cc -- implement Key_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  
  Todo: key undo, special keys.
  
  */
#include "key-grav.hh"
#include "key-item.hh"
#include "command-request.hh"
#include "local-key-grav.hh"
#include "musical-request.hh"
#include "local-key-item.hh"
#include "bar.hh"
#include "time-description.hh"

Key_engraver::Key_engraver()
{
  kit_p_ = 0;
  do_post_move_processing();
}

void
Key_engraver::create_key()
{
  if (!kit_p_) 
    {
      int c0_i=0;

      Staff_info inf = get_staff_info();
      if (inf.c0_position_i_l_)
	c0_i = *get_staff_info().c0_position_i_l_;	
	
      kit_p_ = new Key_item (c0_i);
      kit_p_->break_priority_i_ = -1; // ugh
      announce_element (Score_elem_info (kit_p_,keyreq_l_));
      kit_p_->read (*this);
    }
}

bool
Key_engraver::do_try_request (Request * req_l)
{
  Command_req* creq_l= req_l->command();
  if (!creq_l|| !creq_l->keychange())
    return false;
   
  if (keyreq_l_)
    return false;		// TODO
  keyreq_l_ = creq_l->keychange();
  read_req (keyreq_l_);
  return true;
}

void
Key_engraver::acknowledge_element (Score_elem_info info)
{
  Command_req * r_l = info.req_l_->command() ;
  if (r_l && r_l->clefchange()) 
    {
      create_key();
    }
  else if (info.elem_l_->is_type_b (Bar::static_name ())) 
    {
      if (!keyreq_l_)
	default_key_b_ = true;
      create_key();
    }

}

void
Key_engraver::do_process_requests()
{
  if (key_.multi_octave_b_) 
    {
      assert (false); // TODO . 
    }
  else if (keyreq_l_) 
    {
      create_key();
    }
}

void
Key_engraver::do_pre_move_processing()
{ 
  if (kit_p_) 
    {
      kit_p_->default_b_ = default_key_b_;
      typeset_element (kit_p_);
      kit_p_ = 0;
    }
}


  
void
Key_engraver::read_req (Key_change_req * r)
{
  key_.clear ();
  key_.multi_octave_b_ = r->multi_octave_b_;
  accidental_idx_arr_.clear();

  for (int i = 0; i < r->melodic_p_arr_.size(); i ++) 
    {
      Melodic_req *  m_l =r->melodic_p_arr_[i];
      int n_i =m_l->notename_i_;
      int a_i = m_l->accidental_i_;
      int o_i = m_l->octave_i_;
      if (r->multi_octave_b_)
	key_.set (o_i, n_i, a_i);
      else
	key_.set (n_i, a_i);
      accidental_idx_arr_.push (n_i);
    }
}

void
Key_engraver::do_post_move_processing()
{
  keyreq_l_ = 0;
  default_key_b_ = false;
}


IMPLEMENT_IS_TYPE_B1(Key_engraver,Engraver);
ADD_THIS_TRANSLATOR(Key_engraver);
