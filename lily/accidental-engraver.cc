/*
  accidental-engraver.cc -- implement accidental_engraver

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Modified 2001-2002 by Rune Zedeler <rz@daimi.au.dk>
*/

#include "musical-request.hh"
#include "command-request.hh"
#include "local-key-item.hh"
#include "item.hh"
#include "tie.hh"
#include "rhythmic-head.hh"
#include "engraver-group-engraver.hh"

#include "staff-symbol-referencer.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "arpeggio.hh"
#include "warn.hh"

#include "translator-group.hh"

/**


FIXME: should not compute vertical positioning of accidentals, but
get them from the noteheads

The algorithm for accidentals should be documented, and made
tweakable.

*/


struct Accidental_engraver : Engraver {
  Item *key_item_p_;
protected:
  TRANSLATOR_DECLARATIONS (Accidental_engraver);
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void initialize ();
  virtual void create_grobs ();
  virtual void finalize ();
public:

  /*
    TODO -> property.
    
    This is not a property, and it is not protected.  This poses a
    very small risk of the value being GC'd from under us.

    Oh well.
  */
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
  Link_array<Translator_group> origin_l_arr_;

};


Accidental_engraver::Accidental_engraver ()
{
  key_item_p_ = 0;
  last_keysig_ = SCM_EOL;
}

void
Accidental_engraver::initialize ()
{
  last_keysig_ = get_property ("keySignature");

  Translator_group * trans_ = daddy_trans_l_;
  while (trans_)
    {
      trans_ -> set_property ("localKeySignature",  ly_deep_copy (last_keysig_));
      trans_ = trans_->daddy_trans_l_;
    }
  daddy_trans_l_->set_children_property ("localKeySignature", last_keysig_);
}

/*

calculates the number of accidentals on basis of the current local key sig
  (passed as argument)
  Returns number of accidentals (0, 1 or 2).
    Negative (-1 or -2) if accidental has changed.

*/
static int
number_accidentals (SCM sig, Note_req * note_l, SCM curbarnum, SCM lazyness, 
		    bool ignore_octave_b)
{
  Pitch *pitch = unsmob_pitch (note_l->get_mus_property ("pitch"));
  int n = pitch->notename_i_;
  int o = pitch->octave_i_;
  int a = pitch->alteration_i_;
  int curbarnum_i = gh_scm2int (curbarnum);
  int accbarnum_i = 0;

  SCM prev;
  if (ignore_octave_b)
    prev = ly_assoc_cdr (gh_int2scm (n), sig);
  else
    prev = gh_assoc (gh_cons (gh_int2scm (o), gh_int2scm (n)), sig);

  /* should really be true unless prev == SCM_BOOL_F */
  if (gh_pair_p (prev) && gh_pair_p (ly_cdr (prev)))
    {
      accbarnum_i = gh_scm2int (ly_cddr (prev));
      prev = gh_cons (ly_car (prev), ly_cadr (prev));
    }
  
  /* If an accidental was not found or the accidental was too old */
  if (prev == SCM_BOOL_F ||
      (gh_number_p (lazyness) && curbarnum_i > accbarnum_i + gh_scm2int (lazyness)))
    prev = gh_assoc (gh_int2scm (n), sig);


  SCM prev_acc = (prev == SCM_BOOL_F) ? gh_int2scm (0) : ly_cdr (prev);

  int p = gh_number_p (prev_acc) ? gh_scm2int (prev_acc) : 0;

  int num;
  if (a == p
      && !to_boolean (note_l->get_mus_property ("force-accidental"))
      && gh_number_p (prev_acc))
    num = 0;
  else if ( (abs (a)<abs (p) || p*a<0) && a != 0 )
    num = 2;
  else
    num = 1;
  
  return a == p ? num : -num;
}

static int
number_accidentals (Note_req * note_l, Translator_group * origin_l, 
		    SCM accidentals, SCM curbarnum)
{
  int number = 0;

  bool diff = false;
  if (gh_pair_p (accidentals) && !gh_symbol_p (ly_car (accidentals)))
    warning (_f ("Accidental typesetting list must begin with context-name: %s", 
		 ly_scm2string (ly_car (accidentals)).ch_C ()));
  
  while (gh_pair_p (accidentals) && origin_l)
    {
      // If pair then it is a new accidentals typesetting rule to be checked
      if (gh_pair_p (ly_car (accidentals)))
	{
	  SCM type = gh_caar (accidentals);
	  SCM lazyness = gh_cdar (accidentals);
	  SCM localsig = origin_l->get_property ("localKeySignature");
	  
	  bool same_octave_b = 
	    gh_eq_p (ly_symbol2scm ("same-octave"), type);
	  bool any_octave_b = 
	    gh_eq_p (ly_symbol2scm ("any-octave"), type);

	  if (same_octave_b || any_octave_b)
	    {
	      int n = number_accidentals
		(localsig, note_l, curbarnum, lazyness, any_octave_b);
	      diff = diff || (n < 0);
	      number = max (number, abs (n));     
	    }
	  else
	    warning (_f ("unknown accidental typesetting: %s. Ignored", 
			 ly_symbol2string (type).ch_C ()));
	}
      

      /*
	if symbol then it is a context name. Scan parent contexts to find it.
      */
      else if (gh_symbol_p (ly_car (accidentals)))
	{
	  String context = ly_symbol2string (ly_car (accidentals));
	  
	  while (origin_l && !origin_l->is_alias_b (context))
	    origin_l = origin_l->daddy_trans_l_;
      
	  if (!origin_l)
	    warning (_f ("Symbol is not a parent context: %s. Ignored", 
			 context.ch_C ()));
	}
      else warning (_f ("Accidental typesetting must be pair or context-name: %s", 
			ly_scm2string (ly_car (accidentals)).ch_C ()));
      
      accidentals = ly_cdr (accidentals);
    }
  return diff ? -number : number;
}

void
Accidental_engraver::create_grobs ()
{
  if (!key_item_p_ && mel_l_arr_.size ()) 
    {
      //SCM localsig = get_property ("localKeySignature");
      SCM accidentals =  get_property ("autoAccidentals");
      SCM cautionaries =  get_property ("autoCautionaries");
      SCM barnum = get_property ("currentBarNumber");

      bool extra_natural_b = get_property ("extraNatural") == SCM_BOOL_T;
      for (int i = 0; i  < mel_l_arr_.size (); i++) 
	{
	  Grob * support_l = head_l_arr_[i];
	  Note_req * note_l = mel_l_arr_[i];
	  Translator_group * origin_l = origin_l_arr_[i];

	  int num = number_accidentals (note_l, origin_l, accidentals, barnum);
	  int num_caut = number_accidentals (note_l, origin_l, cautionaries, barnum);
	  bool cautionary = to_boolean (note_l->get_mus_property ("cautionary"));
	  
	  if (abs (num_caut)>abs (num))
	    {
	      num = num_caut;
	      cautionary = true;
	    }
	  
	  bool different = num < 0;
	  num = abs (num);

	  /* see if there's a tie that "changes" the accidental */
	  /* works because if there's a tie, the note to the left
	     is of the same pitch as the actual note */

	  Grob *tie_break_reminder = 0;
	  bool tie_changes = false;
	  for (int j = 0; j < tie_l_arr_.size (); j++)
	    if (support_l == Tie::head (tie_l_arr_[j], RIGHT))
	      {
		tie_changes = different;

		/* Enable accidentals for broken tie

		We only want an accidental on a broken tie, 
		if the tie changes the accidental.
		   
		Maybe check property noTieBreakForceAccidental? */
		if (different)
		  tie_break_reminder = tie_l_arr_[j];
		break;
	      }

	  if (num)
	    {
	      if (!key_item_p_) 
		{
		  key_item_p_ = new Item (get_property ("Accidentals"));

		  SCM c0 = get_property ("centralCPosition");
		  if (gh_number_p (c0))
		    Staff_symbol_referencer::set_position (key_item_p_, gh_scm2int (c0));
			 
		  announce_grob (key_item_p_, SCM_EOL);
		}

	      
	      Local_key_item::add_pitch (key_item_p_, *unsmob_pitch (note_l->get_mus_property ("pitch")), 
					 cautionary, 
					 num == 2 && extra_natural_b, 
					 tie_break_reminder);
	      Side_position_interface::add_support (key_item_p_, support_l);
	      
	      support_l->set_grob_property ("accidental-grob", key_item_p_->self_scm ());
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
	  SCM on_s = gh_cons (gh_int2scm (o), gh_int2scm (n));

	  /*
	    TODO: Speed this up!
	    
	    Perhaps only check translators mentioned in the auto-accidentals?
	    -rz

	    TODO: profile this.
	    
	    I'd be surprised if the impact of this would be
	    measurable.  Anyway, it seems localsig doesn't change
	    every time-step, but a set_property() is done every
	    time. We could save on that, probably.

	    --hwn.
	    
	    
	  */
	  Translator_group * trans_ = origin_l_arr_[i];
	  while (trans_)
	    {
	      SCM localsig = trans_->get_property ("localKeySignature");
	      if (tie_changes)
		{
		  /*
		    Remember an alteration that is different both from
		    that of the tied note and of the key signature.
		  */
		  localsig = ly_assoc_front_x
		    (localsig, on_s, gh_cons (SCM_BOOL_T, barnum));
		}
	      else
		{
		  /*
		    not really really correct if there are more than one
		    noteheads with the same notename.
		  */
		  localsig = ly_assoc_front_x
		    (localsig, on_s, gh_cons (gh_int2scm (a), barnum)); 
		}
	      trans_->set_property ("localKeySignature",  localsig);
	      trans_ = trans_->daddy_trans_l_;
	    }
	}
    }

  
  if (key_item_p_)
    {
      /*
	We add the accidentals to the support of the arpeggio, so it is put left of the
	accidentals. 
	
      */
      for (int i = 0;  i < arpeggios_.size ();  i++)
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
      for (int i = 0; i < head_l_arr_.size (); i++)
	Side_position_interface::add_support (key_item_p_, head_l_arr_[i]);

      typeset_grob (key_item_p_);
      key_item_p_ = 0;
    }


  mel_l_arr_.clear ();
  arpeggios_.clear ();
  tie_l_arr_.clear ();
  head_l_arr_.clear ();
  forced_l_arr_.clear ();
  origin_l_arr_.clear ();
}

void
Accidental_engraver::acknowledge_grob (Grob_info info)
{
  Note_req * note_l =  dynamic_cast <Note_req *> (info.music_cause ());

  if (note_l && Rhythmic_head::has_interface (info.grob_l_))
    {
      mel_l_arr_.push (note_l);
      head_l_arr_.push (info.grob_l_);
      origin_l_arr_.push (info.origin_trans_l_->daddy_trans_l_);
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

void
Accidental_engraver::process_music ()
{
  SCM sig = get_property ("keySignature");

  /* Detect key sig changes.
     Update all parents and children
  */
  if (last_keysig_ != sig)
    {
      Translator_group * trans_ = daddy_trans_l_;
      while (trans_)
	{
	  trans_ -> set_property ("localKeySignature",  ly_deep_copy (sig));
	  trans_ = trans_->daddy_trans_l_;
	}
      daddy_trans_l_->set_children_property ("localKeySignature", sig);

      last_keysig_ = sig;
    }
}





ENTER_DESCRIPTION (Accidental_engraver,
"Make accidentals.  Catches note heads, ties and notices key-change
events.  Due to interaction with ties (which don't come together
with note heads), this needs to be in a context higher than Tie_engraver.",
		   
	       "Accidentals",
	       "rhythmic-head-interface tie-interface arpeggio-interface",
	       "localKeySignature extraNatural autoAccidentals autoCautionaries",
		   "localKeySignature");
