/*
  accidental-engraver.cc -- implement accidental_engraver

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Modified 2001 by Rune Zedeler <rz@daimi.au.dk>
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
#include "warn.hh"

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
  Link_array<Grob> head_l_arr_;
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

/** calculates the number of accidentals on basis of the current local key sig
  * (passed as argument).
  * Returns number of accidentals (0, 1 or 2).
  *   Negative (-1 or -2) if accidental has changed.
  **/
static int
number_accidentals (SCM sig, Note_req * note_l, SCM curbarnum, SCM lazyness,
		    bool ignore_octave_b)
{
  Pitch *pitch = unsmob_pitch (note_l->get_mus_property ("pitch"));
  int n = pitch->notename_i_;
  int o = pitch->octave_i_;
  int a = pitch->alteration_i_;
  int curbarnum_i = gh_scm2int(curbarnum);
  int accbarnum_i = 0;
  SCM prev;
  if (ignore_octave_b)
    prev = ly_assoc_cdr (gh_int2scm (n), sig);
  else
    prev = gh_assoc (gh_cons (gh_int2scm (o), gh_int2scm (n)), sig);
  /* should really be true unless prev==SCM_BOOL_F */
  if(gh_pair_p(prev) && gh_pair_p(ly_cdr(prev))) {
    accbarnum_i = gh_scm2int(ly_cddr(prev));
    prev = gh_cons(ly_car(prev),ly_cadr(prev));
  }
  /* If an accidental was not found or the accidental was too old */
  if (prev == SCM_BOOL_F ||
      (gh_number_p(lazyness) && curbarnum_i>accbarnum_i+gh_scm2int(lazyness)))
    prev = gh_assoc (gh_int2scm (n), sig);
  SCM prev_acc = (prev == SCM_BOOL_F) ? gh_int2scm (0) : ly_cdr (prev);

  int p = gh_number_p (prev_acc) ? gh_scm2int (prev_acc) : 0;

  int num;
  if (a==p && !to_boolean (note_l->get_mus_property ("force-accidental")) && gh_number_p(prev_acc)) num=0;
  else if ( (abs(a)<abs(p) || p*a<0) && a!=0 ) num=2;
  else num=1;
  
  return a==p ? num : -num;
}

static int
number_accidentals (SCM localsig, Note_req * note_l, SCM accidentals_l,
		    SCM curbarnum) {
  int number=0;
  int diff=0;
  while(gh_pair_p(accidentals_l)) {
    if(gh_pair_p(ly_car(accidentals_l))) {
      SCM type = gh_caar(accidentals_l);
      SCM lazyness = gh_cdar(accidentals_l);
      bool measure_same_octave_b =
	gh_eq_p(ly_symbol2scm("measure-same-octave"),type);
      bool measure_any_octave_b =
	gh_eq_p(ly_symbol2scm("measure-any-octave"),type);
      if(measure_same_octave_b || measure_any_octave_b) {
	int n = number_accidentals
	  (localsig,note_l,curbarnum,lazyness,measure_any_octave_b);
	diff |= n<0;
	number = max(number,abs(n));     
      }
      else warning(_f("unknown accidental typesetting: %s",
		      ly_symbol2string(type).ch_C()));
    }
    else warning(_f("Accidental typesetting must be pair: %s",
		      ly_scm2string(ly_car(accidentals_l)).ch_C()));
    accidentals_l = ly_cdr(accidentals_l);
  }
  return diff ? -number : number;
}

void
Accidental_engraver::create_grobs ()
{
  if (!key_item_p_ && mel_l_arr_.size ()) 
    {
      SCM localsig = get_property ("localKeySignature");
      SCM accidentals_l =  get_property ("autoAccidentals");
      SCM cautionaries_l =  get_property ("autoCautionaries");
      SCM barnum = get_property ("currentBarNumber");

      bool extra_natural_b = get_property ("extraNatural")==SCM_BOOL_T;
      for (int i=0; i  < mel_l_arr_.size (); i++) 
	{
	  Grob * support_l = head_l_arr_[i];
	  Note_req * note_l = mel_l_arr_[i];

	  int num = number_accidentals(localsig,note_l,accidentals_l,barnum);
	  int num_caut = number_accidentals(localsig,note_l,cautionaries_l,barnum);
	  bool cautionary = to_boolean (note_l->get_mus_property ("cautionary"));
	  if (abs(num_caut)>abs(num))
	    {
	      num=num_caut;
	      cautionary=true;
	    }
	  
	  bool different=num<0;
	  num=abs(num);

	  /* see if there's a tie that "changes" the accidental */
	  /* works because if there's a tie, the note to the left
	     is of the same pitch as the actual note */


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

	  if (num)
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

	      
	      Local_key_item::add_pitch (key_item_p_, *unsmob_pitch (note_l->get_mus_property ("pitch")),
					 cautionary,
					 num==2 && extra_natural_b,
					 tie_break_reminder);
	      Side_position_interface::add_support (key_item_p_,support_l);
	      
	      support_l->set_grob_property ("accidentals", key_item_p_->self_scm ());
	    }
	  

	  /*
	    We should not record the accidental if it is the first
	    note and it is tied from the previous measure.

	    Checking whether it is tied also works mostly, but will it
	    always do the correct thing?
	   */
	  
	  Pitch *pitch = unsmob_pitch (note_l->get_mus_property ("pitch"));
	  int n = pitch->notename_i_;
	  int o = pitch->octave_i_;
	  int a = pitch->alteration_i_;
	  SCM on = gh_cons (gh_int2scm (o), gh_int2scm (n));
	  if (tie_changes)
	    {
	      /*
		Remember an alteration that is different both from
		that of the tied note and of the key signature.
	       */
	      localsig = ly_assoc_front_x
		(localsig, on, gh_cons(SCM_BOOL_T,barnum));
	    }
	  else
	    {
	      /*
		not really really correct if there are more than one
		noteheads with the same notename.
	       */
	      localsig = ly_assoc_front_x
		(localsig, on, gh_cons(gh_int2scm (a),barnum)); 
	    }
        }
  
      daddy_trans_l_->set_property ("localKeySignature",  localsig);
    }
  
  
  if (key_item_p_)
    {
      /*
	We add the accidentals to the support of the arpeggio, so it is put left of the
	accidentals. 
	
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
      for (int i=0; i < head_l_arr_.size (); i++)
	Side_position_interface::add_support (key_item_p_,head_l_arr_[i]);

      typeset_grob (key_item_p_);
      key_item_p_ =0;
    }


  mel_l_arr_.clear ();
  arpeggios_.clear ();
  tie_l_arr_.clear ();
  head_l_arr_.clear ();
  forced_l_arr_.clear ();	
}

void
Accidental_engraver::acknowledge_grob (Grob_info info)
{
  Note_req * note_l =  dynamic_cast <Note_req *> (info.music_cause ());

  if (note_l && Rhythmic_head::has_interface (info.grob_l_))
    {
      mel_l_arr_.push (note_l);
      head_l_arr_.push (info.grob_l_);
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

  /* Detect key sig changes. */
  if (last_keysig_ != sig) 
    {
      daddy_trans_l_->set_property ("localKeySignature",  ly_deep_copy (sig));
      last_keysig_ = sig;
    }
}





ENTER_DESCRIPTION(Accidental_engraver,
/* descr */       "Make accidentals.  Catches note heads, ties and notices key-change
events.  Due to interaction with ties (which don't come together
with note heads), this needs to be in a context higher than Tie_engraver. FIXME",
/* creats*/       "Accidentals",
/* acks  */       "rhythmic-head-interface tie-interface arpeggio-interface",
/* reads */       "localKeySignature extraNatural autoAccidentals autoCautionaries",
/* write */       "localKeySignature");
