/*
  local-key-reg.cc -- implement Local_key_engraver

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musical-request.hh"
#include "command-request.hh"
#include "local-key-grav.hh"
#include "local-key-item.hh"
#include "key-grav.hh"
#include "debug.hh"
#include "key-item.hh"
#include "tie.hh"
#include "note-head.hh"
#include "time-description.hh"
#include "engraver-group.hh"


Local_key_engraver::Local_key_engraver()
{
  key_C_ = 0;
}

void
Local_key_engraver::do_creation_processing ()
{
  Translator * result =
    daddy_grav_l()->get_simple_translator (Key_engraver::static_name());

  if (!result)
    {
      warning ("Out of tune! Can't find key engraver");
    }
  else
    {
      key_C_ = &((Key_engraver*)result->engraver_l ())->key_;
      local_key_ = *key_C_;
    }
}

void
Local_key_engraver::do_pre_move_processing()
{
  Local_key_item *key_item_p = 0;
  if (mel_l_arr_.size()) 
    {
      for (int i=0; i  < mel_l_arr_.size(); i++) 
	{
	  Item * support_l = support_l_arr_[i];
	  Note_req * note_l = mel_l_arr_[i];

	  if (tied_l_arr_.find_l (support_l) && 
	      !note_l->forceacc_b_)
	    continue;
	    
	  if (!note_l->forceacc_b_ &&
	      local_key_.oct (note_l->octave_i_).acc (note_l->notename_i_)
	      == note_l->accidental_i_) 
	    continue;
	  if (!key_item_p) 
	    {
	      int c0_i=0;

	      Staff_info inf = get_staff_info();
	      if (inf.c0_position_i_l_)
		c0_i = *get_staff_info().c0_position_i_l_;	
		
	      key_item_p = new Local_key_item (c0_i);

	    }
	  key_item_p->add (note_l);
	  key_item_p->add_support (support_l);
	  local_key_.oct (note_l->octave_i_)
	    .set (note_l->notename_i_, note_l->accidental_i_);
	}
	
    }
  if (key_item_p) 
    {
      for (int i=0; i < support_l_arr_.size(); i++)
	key_item_p->add_support (support_l_arr_[i]);
	
      announce_element (Score_elem_info (key_item_p, 0)); // ugh ugh ugh
      typeset_element (key_item_p);
    }
  
  mel_l_arr_.clear();
  tied_l_arr_.clear();
  support_l_arr_.clear();
  forced_l_arr_.clear();	
}

/*
  whoah .. this looks hairy!
 */
void
Local_key_engraver::acknowledge_element (Score_elem_info info)
{    
  Score_elem * elem_l = info.elem_l_;
  if (info.req_l_->musical() && info.req_l_->musical ()->note ()) 
    {
      Note_req * note_l = info.req_l_->musical()->note ();
      Item * item_l = info.elem_l_->item();

      mel_l_arr_.push (note_l);
      support_l_arr_.push (item_l);
    }
  else if (info.req_l_->command()
	   && info.req_l_->command()->keychange ()) 
    {
      local_key_ = *key_C_;
    }
  else if (elem_l->is_type_b (Tie::static_name ())) 
    {
      Tie * tie_l = (Tie*)elem_l->spanner();
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
