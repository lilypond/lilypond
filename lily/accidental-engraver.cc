/*
  local-key-engraver.cc -- implement accidental_engraver

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "command-request.hh"
#include "local-key-item.hh"
#include "item.hh"
#include "tie.hh"
#include "rhythmic-head.hh"
#include "timing-translator.hh"
#include "engraver-group-engraver.hh"

#include "staff-symbol-referencer.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "arpeggio.hh"

/**


   FIXME: should not compute vertical positioning of accidentals, but
   get them from the noteheads

   The algorithm for accidentals should be documented, and made
   tweakable.

*/


struct Accidental_engraver : Engraver {
  Item *key_item_p_;
protected:
  TRANSLATOR_DECLARATIONS(Accidental_engraver);
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void initialize ();
  virtual void create_grobs ();
  virtual void finalize ();
public:

  // todo -> property
  SCM last_keysig_;

  /*
    Urgh. Since the accidentals depend on lots of variables, we have to
    store all information before we can really create the accidentals.
   */
  Link_array<Grob> arpeggios_;
  
  Link_array<Note_req> mel_l_arr_;
  Link_array<Grob> support_l_arr_;
  Link_array<Item> forced_l_arr_;
  Link_array<Grob> tie_l_arr_;

};

Accidental_engraver::Accidental_engraver ()
{
  key_item_p_ =0;
  last_keysig_ = SCM_EOL;
}

void
Accidental_engraver::initialize ()
{
  last_keysig_ = get_property ("keySignature");
  daddy_trans_l_->set_property ("localKeySignature",  last_keysig_);  
}

void
Accidental_engraver::create_grobs ()
{
  if (!key_item_p_ && mel_l_arr_.size ()) 
    {
      SCM localsig = get_property ("localKeySignature");
  
      for (int i=0; i  < mel_l_arr_.size (); i++) 
	{
	  Grob * support_l = support_l_arr_[i];
	  Note_req * note_l = mel_l_arr_[i];

	  int n = unsmob_pitch (note_l->get_mus_property ("pitch"))->notename_i_;
	  int o = unsmob_pitch (note_l->get_mus_property ("pitch"))->octave_i () ;
	  int a = unsmob_pitch (note_l->get_mus_property ("pitch"))->alteration_i_;
	  
	  /* see if there's a tie that "changes" the accidental */
	  /* works because if there's a tie, the note to the left
	     is of the same pitch as the actual note */

	  SCM prev = scm_assoc (gh_cons (gh_int2scm (o), gh_int2scm (n)), localsig);
	  if (prev == SCM_BOOL_F)
	    prev = scm_assoc (gh_int2scm (n), localsig);
	  SCM prev_acc = (prev == SCM_BOOL_F) ? gh_int2scm (0) : ly_cdr (prev);
	  bool different = !gh_equal_p (prev_acc , gh_int2scm (a));
	  int p = gh_number_p (prev_acc) ? gh_scm2int (prev_acc) : 0;

	  Grob *tie_break_reminder = 0;
	  bool tie_changes = false;
	  for (int i=0; i < tie_l_arr_.size (); i++)
	    if (support_l == Tie::head (tie_l_arr_[i], RIGHT))
	      {
		tie_changes = different;
		/* Enable accidentals for broken tie

		   We only want an accidental on a broken tie,
		   if the tie changes the accidental.
		   
		   Maybe check property noTieBreakForceAccidental? */
		if (different)
		  tie_break_reminder = tie_l_arr_[i];
		break;
	      }

	  /* When do we want accidentals:

	     1. when property force-accidental is set, and not
	     tie_changes
	     2. when different and not tie-changes
	     3. maybe when at end of a tie: we must later see if
	     we're after a line break */
	  if (( (to_boolean (note_l->get_mus_property ("force-accidental"))
		|| different)
	       && !tie_changes)
	      || tie_break_reminder)
	    {
	      if (!key_item_p_) 
		{
		  key_item_p_ = new Item (get_property ("Accidentals"));
		  Local_key_item::set_interface (key_item_p_);

		  
		  Staff_symbol_referencer::set_interface (key_item_p_);
		  SCM c0 = get_property ("centralCPosition");
		  if (gh_number_p (c0))
		    Staff_symbol_referencer::set_position (key_item_p_, gh_scm2int (c0));
			 
		  announce_grob (key_item_p_, 0);
		}

	      
	      bool extra_natural =
		sign (p) * (p - a) == 1
		&& abs (p) == 2;

	      Local_key_item::add_pitch (key_item_p_, *unsmob_pitch (note_l->get_mus_property ("pitch")),
					 to_boolean (note_l->get_mus_property ("cautionary")),
					 extra_natural,
					 tie_break_reminder);
	      Side_position_interface::add_support (key_item_p_,support_l);
	    }
	  
	  /*
	    We should not record the accidental if it is the first
	    note and it is tied from the previous measure.

	    Checking whether it is tied also works mostly, but will it
	    always do the correct thing?

	   */
	  bool forget = to_boolean (get_property ("forgetAccidentals"));
	  if (tie_changes)
	    {
	      /*
		Remember an alteration that is different both from
		that of the tied note and of the key signature.

	       */
	      localsig = scm_assoc_set_x (localsig, gh_cons (gh_int2scm (o),
							     gh_int2scm (n)),
					  SCM_BOOL_T); 

	    }
	  else if (!forget)
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
  

  if (key_item_p_)
    {
      /*
	Hmm. Which one has to be on the left?

	On which left, code or paper?

 (Arpeggios are engraved left of accidentals, of course.)
       */
      for (int i=0;  i < arpeggios_.size ();  i++)
	Side_position_interface::add_support (arpeggios_[i], key_item_p_);

      arpeggios_.clear ();
    }
}

void
Accidental_engraver::finalize ()
{

}

void
Accidental_engraver::stop_translation_timestep ()
{
  if (key_item_p_)
    {
      for (int i=0; i < support_l_arr_.size (); i++)
	Side_position_interface::add_support (key_item_p_,support_l_arr_[i]);

      typeset_grob (key_item_p_);
      key_item_p_ =0;
    }


  mel_l_arr_.clear ();
  arpeggios_.clear ();
  tie_l_arr_.clear ();
  support_l_arr_.clear ();
  forced_l_arr_.clear ();	
}

void
Accidental_engraver::acknowledge_grob (Grob_info info)
{
  Note_req * note_l =  dynamic_cast <Note_req *> (info.req_l_);

  if (note_l && Rhythmic_head::has_interface (info.grob_l_))
    {
      mel_l_arr_.push (note_l);
      support_l_arr_.push (info.grob_l_);
    }
  else if (Tie::has_interface (info.grob_l_))
    {
      tie_l_arr_.push (info.grob_l_);
    }
  else if (Arpeggio::has_interface (info.grob_l_))
    {
      arpeggios_.push (info.grob_l_); 
    }
  
}

/*
  ugh. repeated deep_copy generates lots of garbage.
 */
void
Accidental_engraver::process_music ()
{
  SCM smp = get_property ("measurePosition");
  Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

  SCM sig = get_property ("keySignature");

  /*
    Detect key sig changes. If we haven't found any, check if at start
    of measure, and set localKeySignature anyhow.  */
  if (last_keysig_ != sig) 
    {
      daddy_trans_l_->set_property ("localKeySignature",  ly_deep_copy (sig));
      last_keysig_ = sig;
    }
  else if (!mp.to_bool () )
    {
      if (!to_boolean (get_property ("noResetKey")))
	daddy_trans_l_->set_property ("localKeySignature",  ly_deep_copy (sig));
    }
}





ENTER_DESCRIPTION(Accidental_engraver,
/* descr */       "Make accidentals.  Catches note heads, ties and notices key-change
events.  Due to interaction with ties (which don't come together
with note heads), this needs to be in a context higher than Tie_engraver. FIXME",
/* creats*/       "Accidentals",
/* acks  */       "rhythmic-head-interface tie-interface arpeggio-interface",
/* reads */       "localKeySignature forgetAccidentals noResetKey",
/* write */       "");
