/*
  local-key-reg.cc -- implement Local_key_engraver

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
#include "engraver-group-engraver.hh"


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
    daddy_grav_l()->get_simple_translator ("Key_engraver");

  if (!result)
    {
      warning (_ ("out of tune") + "! " + _ ("can't find") + " Key_engraver");
    }
  else
    {
      key_C_ = &(dynamic_cast<Key_engraver *> (result))->key_;
      local_key_ = *key_C_;
    }
}

void
Local_key_engraver::process_acknowledged ()
{
  if (!key_item_p_ && mel_l_arr_.size()) 
    {
      bool forget = get_property ("forgetAccidentals",0).to_bool();
      for (int i=0; i  < mel_l_arr_.size(); i++) 
	{
	  Item * support_l = support_l_arr_[i];
	  Note_req * note_l = mel_l_arr_[i];

	  if (tied_l_arr_.find_l (support_l) && 
	      !note_l->forceacc_b_)
	    {
	      if (!forget)
		local_key_.set (note_l->pitch_);
	      continue;
	    }
	    
	  if (!note_l->forceacc_b_
	      && local_key_.different_acc (note_l->pitch_))
	    continue;
	  if (!key_item_p_) 
	    {
	      key_item_p_ = new Local_key_item;
	      announce_element (Score_element_info (key_item_p_, 0));	      
	    }


	  key_item_p_->add_pitch (note_l->pitch_,
				  note_l->cautionary_b_);
	  key_item_p_->add_support (support_l);
	  
	  if (!forget)
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
  Note_req * note_l =  dynamic_cast <Note_req *> (info.req_l_);
  Note_head * note_head = dynamic_cast<Note_head *> (info.elem_l_);
  
  if (note_l && note_head)
    {
      mel_l_arr_.push (note_l);
      support_l_arr_.push (note_head);
    }
  else if (dynamic_cast <Key_change_req*> (info.req_l_))
    {
      local_key_ = *key_C_;
    }
  else if (Tie * tie_l = dynamic_cast<Tie *> (info.elem_l_))
    {
      tied_l_arr_.push (tie_l-> head_l_drul_[RIGHT]);
    }
}

void
Local_key_engraver::do_process_requests()
{
  Time_description const * time_C_ = get_staff_info().time_C_;
  if (time_C_ && !time_C_->whole_in_measure_)
    {
      bool no_res = get_property ("noResetKey",0).to_bool ();
      if (!no_res && key_C_)
	local_key_= *key_C_;
    }
}



ADD_THIS_TRANSLATOR(Local_key_engraver);
