/*
  local-key-engraver.cc -- implement Local_key_engraver

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "command-request.hh"
#include "local-key-item.hh"
#include "key-item.hh"
#include "tie.hh"
#include "note-head.hh"
#include "timing-translator.hh"
#include "engraver-group-engraver.hh"
#include "grace-align-item.hh"
#include "staff-symbol-referencer.hh"
#include "side-position-interface.hh"
#include "engraver.hh"


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
  virtual void do_process_music();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_creation_processing ();
  virtual void process_acknowledged ();
  virtual void do_removal_processing ();
public:

  // todo -> property
  SCM last_keysig_;
  
  Link_array<Note_req> mel_l_arr_;
  Link_array<Item> support_l_arr_;
  Link_array<Item> forced_l_arr_;
  Link_array<Item> tied_l_arr_;
  Local_key_engraver();

  Grace_align_item * grace_align_l_;
};

Local_key_engraver::Local_key_engraver()
{
  key_item_p_ =0;
  grace_align_l_ =0;
  last_keysig_ = SCM_EOL;
}

void
Local_key_engraver::do_creation_processing ()
{
  last_keysig_ = get_property ("keySignature");
  daddy_trans_l_->set_property ("localKeySignature",  last_keysig_);  
}

void
Local_key_engraver::process_acknowledged ()
{
  SCM localsig = get_property ("localKeySignature");
  
  if (!key_item_p_ && mel_l_arr_.size()) 
    {
      SCM f = get_property ("forgetAccidentals");
      bool forget = to_boolean (f);
      for (int i=0; i  < mel_l_arr_.size(); i++) 
	{
	  Item * support_l = support_l_arr_[i];
	  Note_req * note_l = mel_l_arr_[i];

	  int n = note_l->pitch_.notename_i_;
	  int o = note_l->pitch_.octave_i_;
	  int a = note_l->pitch_.accidental_i_;
	  
	  /* see if there's a tie that "changes" the accidental */
	  /* works because if there's a tie, the note to the left
	     is of the same pitch as the actual note */

	  SCM prev = scm_assoc (gh_cons (gh_int2scm (o), gh_int2scm (n)), localsig);
	  if (prev == SCM_BOOL_F)
	    prev = scm_assoc (gh_int2scm (n), localsig);
	  int prev_acc = (prev == SCM_BOOL_F) ? 0 : gh_scm2int (gh_cdr (prev));
	  bool different = prev_acc != a;
	  
	  bool tie_changes = tied_l_arr_.find_l (support_l) && different;
	  if (!forget
	      && (note_l->forceacc_b_ || different)
	      && !tie_changes)
	    {
	      if (!key_item_p_) 
		{
		  key_item_p_ = new Local_key_item (get_property ("basicLocalKeyProperties"));
		  Side_position_interface (key_item_p_).set_axis (X_AXIS);
		  Side_position_interface (key_item_p_).set_direction (LEFT);
		  staff_symbol_referencer(key_item_p_).set_interface ();
			 
		  announce_element (Score_element_info (key_item_p_, 0));
		}

	      
	      bool extra_natural =
		sign (prev_acc) * (prev_acc - a) == 1
		&& abs(prev_acc) == 2;

	      key_item_p_->add_pitch (note_l->pitch_,
	  			      note_l->cautionary_b_,
				      extra_natural);
	      Side_position_interface (key_item_p_).add_support (support_l);
	    }
	  
	  if (!forget)
	    {
	      localsig = scm_assoc_set_x (localsig, gh_cons (gh_int2scm (o),
							     gh_int2scm (n)),
					  gh_int2scm (a)); 

#if 0
	      /*
		TESTME!
	       */
	      if (!tied_l_arr_.find_l (support_l))
		{
		  local_key_.clear_internal_forceacc (note_l->pitch_);
		}
	      else if (tie_changes)
		{
		  local_key_.set_internal_forceacc (note_l->pitch_);
		}
#endif
	    }
        }
    }

  /*
    UGH ! 
   */
  
  if (key_item_p_ && grace_align_l_)
    {
      Side_position_interface (grace_align_l_).add_support (key_item_p_);
      grace_align_l_ =0;
    }
  
}

void
Local_key_engraver::do_removal_processing ()
{
  // TODO: if grace ? signal accidentals to Local_key_engraver the 
}

void
Local_key_engraver::do_pre_move_processing()
{
  if (key_item_p_)
    {
      for (int i=0; i < support_l_arr_.size(); i++)
	Side_position_interface (key_item_p_).add_support (support_l_arr_[i]);

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
  SCM wg= get_property ("weAreGraceContext");
  
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

/*
  ugh. deep_copy uses lots of space.
 */
void
Local_key_engraver::do_process_music()
{
  SCM smp = get_property ("measurePosition");
  Moment mp =  (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

  SCM sig = get_property ("keySignature");
  if (!mp)
    {
      if (!to_boolean (get_property ("noResetKey")))
	daddy_trans_l_->set_property ("localKeySignature",  ly_deep_copy (sig));
    }
  else if (last_keysig_ != sig) 
    {
      daddy_trans_l_->set_property ("localKeySignature",  ly_deep_copy (sig));
    }
}



ADD_THIS_TRANSLATOR(Local_key_engraver);

