/*
  local-key-engraver.cc -- implement Local_key_engraver

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
// clean up!

#include "musical-request.hh"
#include "command-request.hh"
#include "local-key-engraver.hh"
#include "local-key-item.hh"
#include "key-engraver.hh"
#include "debug.hh"
#include "key-item.hh"
#include "tie.hh"
#include "note-head.hh"
#include "timing-translator.hh"
#include "engraver-group-engraver.hh"
#include "grace-align-item.hh"
#include "staff-symbol-referencer.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "key.hh"
#include "parray.hh"


/**
   Make accidentals.  Catches note heads, ties and notices key-change
   events.  Due to interaction with ties (which don't come together
   with note heads), this needs to be in a context higher than Tie_engraver.
   (FIXME).
 */
struct Local_key_engraver : Engraver {
  Local_key_item *key_item_p_;
protected:
  VIRTUAL_COPY_CONS(Translator);
  virtual void do_process_requests();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_creation_processing ();
  virtual void process_acknowledged ();
  virtual void do_removal_processing ();
public:
  
  Key local_key_;
  Key_engraver *key_grav_l_;
  Array<Note_req* > mel_l_arr_;
  Array<Item*> support_l_arr_;
  Link_array<Item  > forced_l_arr_;
  Link_array<Item > tied_l_arr_;
  Local_key_engraver();
  bool self_grace_b_;
  Grace_align_item * grace_align_l_;
  Timing_translator * time_trans_l_  ;
};



Local_key_engraver::Local_key_engraver()
{
  key_grav_l_ = 0;
  key_item_p_ =0;
  grace_align_l_ =0;
  time_trans_l_ = 0;
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

  Translator * tr = daddy_grav_l()->get_simple_translator ("Timing_engraver");	// ugh
  time_trans_l_ = dynamic_cast<Timing_translator*> (tr);
}

void
Local_key_engraver::process_acknowledged ()
{
  if (!key_item_p_ && mel_l_arr_.size()) 
    {
      SCM f = get_property ("forgetAccidentals",0);
      bool forget = to_boolean (f);
      for (int i=0; i  < mel_l_arr_.size(); i++) 
	{
	  Item * support_l = support_l_arr_[i];
	  Note_req * note_l = mel_l_arr_[i];

	  /* see if there's a tie that "changes" the accidental */
	  /* works because if there's a tie, the note to the left
	     is of the same pitch as the actual note */
	  bool tie_changes = tied_l_arr_.find_l (support_l)
	    && !local_key_.different_acc (note_l->pitch_);

	  if (!forget

	      && ((note_l->forceacc_b_
		   || !local_key_.different_acc (note_l->pitch_)
		   || local_key_.internal_forceacc (note_l->pitch_)))

	      && !tie_changes)
	    {
	      if (!key_item_p_) 
		{
		  key_item_p_ = new Local_key_item;
		  side_position (key_item_p_).set_axis (X_AXIS);
		  side_position (key_item_p_).set_direction (LEFT);
		  staff_symbol_referencer(key_item_p_).set_interface ();
			 
		  announce_element (Score_element_info (key_item_p_, 0));
		}

	      key_item_p_->add_pitch (note_l->pitch_,
	  			      note_l->cautionary_b_,
				      local_key_.double_to_single_acc(note_l->pitch_));
	      side_position (key_item_p_).add_support (support_l);
	    }
	  
	  if (!forget)
	    {
	      local_key_.set (note_l->pitch_);
	      if (!tied_l_arr_.find_l (support_l))
		{
		  local_key_.clear_internal_forceacc (note_l->pitch_);
		}
	      else if (tie_changes)
		{
		  local_key_.set_internal_forceacc (note_l->pitch_);
		}
	    }
        }
    }
  if (key_item_p_ && grace_align_l_)
    {
      side_position (grace_align_l_).add_support (key_item_p_);
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
	side_position (key_item_p_).add_support (support_l_arr_[i]);

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
  SCM wg= get_property ("weAreGraceContext", 0);
  
  bool selfgr = gh_boolean_p (wg) &&gh_scm2bool (wg);
  bool he_gr = to_boolean (info.elem_l_->get_elt_property ("grace"));

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
      tied_l_arr_.push (tie_l->head (RIGHT));
    }
}

void
Local_key_engraver::do_process_requests()
{
  if (time_trans_l_ && !time_trans_l_->measure_position ())
    {
      if (!to_boolean (get_property ("noResetKey",0)) && key_grav_l_)
	local_key_= key_grav_l_->key_;
    }
  else if (key_grav_l_ && key_grav_l_->key_changed_b ())
    {
      local_key_ = key_grav_l_->key_;
    }
}



ADD_THIS_TRANSLATOR(Local_key_engraver);

