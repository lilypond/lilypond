/*
  local-key-reg.cc -- implement Local_key_engraver

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "command-request.hh"
#include "local-key-engraver.hh"
#include "local-key-item.hh"
#include "key-engraver.hh"
#include "debug.hh"
#include "key-item.hh"
#include "tie.hh"
#include "note-head.hh"
#include "time-description.hh"
#include "engraver-group.hh"


Local_key_engraver::Local_key_engraver()
{
  key_C_ = 0;
  key_item_p_ =0;
}

void
Local_key_engraver::do_creation_processing ()
{
  /*
    UGHGUHGUH.

    Breaks if Key_engraver is removed from under us.
   */
  Translator * result =
    daddy_grav_l()->get_simple_translator (Key_engraver::static_name());

  if (!result)
    {
      warning (_ ("out of tune") + "! " + _ ("can't find") + " Key_engraver");
    }
  else
    {
      key_C_ = &((Key_engraver*)result->access_Engraver  ())->key_;
      local_key_ = *key_C_;
    }
}

void
Local_key_engraver::process_acknowledged ()
{
  if (!key_item_p_ && mel_l_arr_.size()) 
    {
      for (int i=0; i  < mel_l_arr_.size(); i++) 
	{
	  Item * support_l = support_l_arr_[i];
	  Note_req * note_l = mel_l_arr_[i];

	  if (tied_l_arr_.find_l (support_l) && 
	      !note_l->forceacc_b_)
	    continue;
	    
	  if (!note_l->forceacc_b_
	      && local_key_.different_acc (note_l->pitch_))
	    continue;
	  if (!key_item_p_) 
	    {
	      key_item_p_ = new Local_key_item;
	      announce_element (Score_element_info (key_item_p_, 0));	      
	    }
	  key_item_p_->add (note_l->pitch_);
	  key_item_p_->add_support (support_l);
	  local_key_.set (note_l->pitch_);
	}
	
    }
}

void
Local_key_engraver::do_pre_move_processing()
{
  if (key_item_p_)
    {
      for (int i=0; i < support_l_arr_.size(); i++)
	key_item_p_->add_support (support_l_arr_[i]);

      typeset_element (key_item_p_);
      key_item_p_ =0;
    }
  
  mel_l_arr_.clear();
  tied_l_arr_.clear();
  support_l_arr_.clear();
  forced_l_arr_.clear();	
}

void
Local_key_engraver::acknowledge_element (Score_element_info info)
{    
  Score_element * elem_l = info.elem_l_;
  Item * item_l = elem_l->access_Item();

  Note_req * note_l = (info.req_l_->access_Musical_req ())
    ? info.req_l_->access_Musical_req ()->access_Note_req () : 0;

  if (note_l &&  item_l && item_l->is_type_b (Note_head::static_name ()))
    {
      mel_l_arr_.push (note_l);
      support_l_arr_.push (item_l);
    }
  else if (info.req_l_->access_Command_req ()
	   && info.req_l_->access_Command_req ()->access_Key_change_req () && key_C_) 
    {
      local_key_ = *key_C_;
    }
  else if (elem_l->is_type_b (Tie::static_name ())) 
    {
      Tie * tie_l = (Tie*)elem_l->access_Spanner ();
      if (tie_l->same_pitch_b_)
	tied_l_arr_.push (tie_l-> head_l_drul_[RIGHT]);
    }
}

void
Local_key_engraver::do_process_requests()
{
  Time_description const * time_C_ = get_staff_info().time_C_;
  if (time_C_ && !time_C_->whole_in_measure_)
    {
      if (key_C_)
	local_key_= *key_C_;
    }
}


IMPLEMENT_IS_TYPE_B1(Local_key_engraver,Engraver);
ADD_THIS_TRANSLATOR(Local_key_engraver);
