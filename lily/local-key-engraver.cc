/*
  local-key-engraver.cc -- implement Local_key_engraver

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
#include "grace-align-item.hh"

Local_key_engraver::Local_key_engraver()
{
  key_grav_l_ = 0;
  key_item_p_ =0;
  grace_align_l_ =0;
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

  key_grav_l_ = dynamic_cast<Key_engraver *> (result);

  if (!key_grav_l_)
    {
      warning (_ ("out of tune:"));
      warning (_f ("Can't find: `%s'", "Key_engraver"));
    }
  else
    {
      local_key_ = key_grav_l_->key_;
    }

  /*
    TODO
    (if we are grace) get key info from parent Local_key_engraver
  */
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
  if (key_item_p_ && grace_align_l_)
    {
      grace_align_l_->add_support (key_item_p_);
      grace_align_l_ =0;
    }
  
}

void
Local_key_engraver::do_removal_processing ()
{
  // TODO: signal accidentals to Local_key_engraver the 
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

  grace_align_l_ = 0;
  mel_l_arr_.clear();
  tied_l_arr_.clear();
  support_l_arr_.clear();
  forced_l_arr_.clear();	
}

void
Local_key_engraver::acknowledge_element (Score_element_info info)
{
  bool selfgr = get_property ("weAreGraceContext", 0).to_bool ();
  bool he_gr = info.elem_l_->get_elt_property (grace_scm_sym) != SCM_BOOL_F;

  Grace_align_item * gai = dynamic_cast<Grace_align_item*> (info.elem_l_);  
  if (he_gr && !selfgr && gai)
    {
      grace_align_l_ = gai;
    }
  Note_req * note_l =  dynamic_cast <Note_req *> (info.req_l_);
  Note_head * note_head = dynamic_cast<Note_head *> (info.elem_l_);


  
  if (he_gr != selfgr)
    return;
  
  if (note_l && note_head)
    {
      mel_l_arr_.push (note_l);
      support_l_arr_.push (note_head);
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
      if (!no_res && key_grav_l_)
	local_key_= key_grav_l_->key_;
    }
  else if (key_grav_l_ && key_grav_l_->key_changed_b ())
    {
      local_key_ = key_grav_l_->key_;
    }
}



ADD_THIS_TRANSLATOR(Local_key_engraver);
