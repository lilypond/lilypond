/*
  local-key-engraver.cc -- implement Local_key_engraver

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "command-request.hh"
#include "local-key-item.hh"
#include "item.hh"
#include "tie.hh"
#include "rhythmic-head.hh"
#include "timing-translator.hh"
#include "engraver-group-engraver.hh"
#include "grace-align-item.hh"
#include "staff-symbol-referencer.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "arpeggio.hh"

/**


   FIXME: should not compute vertical positioning of accidentals, but
   get them from the noteheads

*/


struct Local_key_engraver : Engraver {
  Item *key_item_p_;
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

  /*
    Urgh. Since the accidentals depend on lots of variables, we have to
    store all information before we can really create the accidentals.
   */
  Link_array<Score_element> arpeggios_;
  
  Link_array<Note_req> mel_l_arr_;
  Link_array<Score_element> support_l_arr_;
  Link_array<Item> forced_l_arr_;
  Link_array<Score_element> tied_l_arr_;
  Local_key_engraver();

  Item * grace_align_l_;
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
  if (!key_item_p_ && mel_l_arr_.size()) 
    {
      SCM localsig = get_property ("localKeySignature");
  
      for (int i=0; i  < mel_l_arr_.size(); i++) 
	{
	  Score_element * support_l = support_l_arr_[i];
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
	  if ((note_l->forceacc_b_ || different) && !tie_changes)
	    {
	      if (!key_item_p_) 
		{
		  key_item_p_ = new Item(get_property ("Accidentals"));
		  Local_key_item::set_interface (key_item_p_);


		  Staff_symbol_referencer::set_interface (key_item_p_);
			 
		  announce_element (key_item_p_, 0);
		}

	      
	      bool extra_natural =
		sign (prev_acc) * (prev_acc - a) == 1
		&& abs(prev_acc) == 2;

	      Local_key_item::add_pitch (key_item_p_, note_l->pitch_,
					 note_l->cautionary_b_,
					 extra_natural);
	      Side_position::add_support (key_item_p_,support_l);
	    }
	  
	  /*
	    We should not record the accidental if it is the first
	    note and it is tied from the previous measure.

	    Checking whether it is tied also works mostly, but will it
	    always do the correct thing?

	   */
	  bool forget = to_boolean (get_property ("forgetAccidentals"));
	  if (!forget && !tie_changes)
	    {
	      /*
		not really really correct if there are more than one
		noteheads with the same notename.
	       */
	      localsig = scm_assoc_set_x (localsig, gh_cons (gh_int2scm (o),
							     gh_int2scm (n)),
					  gh_int2scm (a)); 

	    }
        }


  
  
      daddy_trans_l_->set_property ("localKeySignature",  localsig);
    }
  
  if (key_item_p_ && grace_align_l_)
    {
      Side_position::add_support (grace_align_l_,key_item_p_);
      grace_align_l_ =0;
    }

  if (key_item_p_)
    {
      /*
	Hmm. Which one has to be on the left?

	On which left, code or paper?

	(Arpeggios are engraved left of accidentals, of course.)
       */
      for (int i=0;  i < arpeggios_.size ();  i++)
	Side_position::add_support (arpeggios_[i], key_item_p_);

      arpeggios_.clear ();
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
	Side_position::add_support (key_item_p_,support_l_arr_[i]);

      typeset_element (key_item_p_);
      key_item_p_ =0;
    }

  grace_align_l_ = 0;
  mel_l_arr_.clear();
  arpeggios_.clear ();
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

  Item * item = dynamic_cast<Item*> (info.elem_l_);  
  if (he_gr && !selfgr && item && Grace_align_item::has_interface (item))
    {
      grace_align_l_ = item;
    }
  if (he_gr != selfgr)
    return;
  
  Note_req * note_l =  dynamic_cast <Note_req *> (info.req_l_);

  if (note_l && Rhythmic_head::has_interface (info.elem_l_))
    {
      mel_l_arr_.push (note_l);
      support_l_arr_.push (info.elem_l_);
    }
  else if (Tie::has_interface (info.elem_l_))
    {
      tied_l_arr_.push (Tie::head (info.elem_l_, RIGHT));
    }
  else if (Arpeggio::has_interface (info.elem_l_))
    {
      arpeggios_.push (info.elem_l_); 
    }
  
}

/*
  ugh. repeated deep_copy generates lots of garbage.
 */
void
Local_key_engraver::do_process_music()
{
  SCM smp = get_property ("measurePosition");
  Moment mp =  (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

  SCM sig = get_property ("keySignature");

  /*
    Detect key sig changes. If we haven't found any, check if at start
    of measure, and set localKeySignature anyhow.  */
  if (last_keysig_ != sig) 
    {
      daddy_trans_l_->set_property ("localKeySignature",  ly_deep_copy (sig));
      last_keysig_ = sig;
    }
  else if (!mp)
    {
      if (!to_boolean (get_property ("noResetKey")))
	daddy_trans_l_->set_property ("localKeySignature",  ly_deep_copy (sig));
    }
}



ADD_THIS_TRANSLATOR(Local_key_engraver);

