/*
  accidental-engraver.cc -- implement accidental_engraver

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Modified 2001--2002 by Rune Zedeler <rz@daimi.au.dk>
*/

#include "event.hh"

#include "item.hh"
#include "tie.hh"
#include "rhythmic-head.hh"
#include "engraver-group-engraver.hh"
#include "accidental-placement.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "arpeggio.hh"
#include "warn.hh"
#include "translator-group.hh"
#include "protected-scm.hh"

/**


FIXME: should not compute vertical positioning of accidentals, but
get them from the noteheads

The algorithm for accidentals should be documented, and made
tweakable.

*/

struct Accidental_entry {
  bool done_;
  Music * melodic_;
  Grob * accidental_;
  Translator_group *origin_;
  Grob*  head_;
  Accidental_entry();
};

Accidental_entry::Accidental_entry()
{
  done_ = false;
  melodic_ =0;
  accidental_ = 0;
  origin_ = 0;
  head_ = 0;
}

struct Accidental_engraver : Engraver {
protected:
  TRANSLATOR_DECLARATIONS (Accidental_engraver);
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void initialize ();
  virtual void process_acknowledged_grobs ();
  virtual void finalize ();
public:

  Protected_scm last_keysig_;

  /*
    Urgh. Since the accidentals depend on lots of variables, we have to
    store all information before we can really create the accidentals.
  */
  Link_array<Grob> left_objects_;
  Link_array<Grob> right_objects_;

  Grob * accidental_placement_;

  /*
    The next 
   */
  Array<Accidental_entry> accidentals_;
  Link_array<Grob> ties_;
};


static void
set_property_on_children (Translator_group * trans, const char * sym, SCM val)
{
  trans->set_property (sym, val);
  for (SCM p = trans->trans_group_list_; gh_pair_p (p); p = ly_cdr(p)) {
    Translator_group *trg =  dynamic_cast<Translator_group*> (unsmob_translator (ly_car (p)));
    set_property_on_children(trg,sym,ly_deep_copy(val));
  }
}

Accidental_engraver::Accidental_engraver ()
{
  accidental_placement_ = 0;
  last_keysig_ = SCM_EOL;
}

void
Accidental_engraver::initialize ()
{
  last_keysig_ = get_property ("keySignature");

  Translator_group * trans_ = daddy_trans_;
  while (trans_)
    {
      trans_ -> set_property ("localKeySignature",  ly_deep_copy (last_keysig_));
      trans_ = trans_->daddy_trans_;
    }
  set_property_on_children (daddy_trans_,"localKeySignature", last_keysig_);
}

/*

calculates the number of accidentals on basis of the current local key sig
  (passed as argument)
  Returns number of accidentals (0, 1 or 2).
    Negative (-1 or -2) if accidental has changed.

*/
static int
number_accidentals_from_sig (SCM sig, Music *, Pitch *pitch, SCM curbarnum, SCM lazyness, 
		    bool ignore_octave_b)
{
  int n = pitch->get_notename ();
  int o = pitch->get_octave();
  int a = pitch->get_alteration ();
  int curbarnum_i = gh_scm2int (curbarnum);
  int accbarnum_i = 0;

  SCM prev;
  if (ignore_octave_b)
    prev = ly_assoc_cdr (scm_int2num (n), sig);
  else
    prev = scm_assoc (gh_cons (scm_int2num (o), scm_int2num (n)), sig);

  /* should really be true unless prev == SCM_BOOL_F */
  if (gh_pair_p (prev) && gh_pair_p (ly_cdr (prev)))
    {
      accbarnum_i = gh_scm2int (ly_cddr (prev));
      prev = gh_cons (ly_car (prev), ly_cadr (prev));
    }
  
  /* If an accidental was not found or the accidental was too old */
  if (prev == SCM_BOOL_F ||
      (gh_number_p (lazyness) && curbarnum_i > accbarnum_i + gh_scm2int (lazyness)))
    prev = scm_assoc (scm_int2num (n), sig);


  SCM prev_acc = (prev == SCM_BOOL_F) ? scm_int2num (0) : ly_cdr (prev);

  int p = gh_number_p (prev_acc) ? gh_scm2int (prev_acc) : 0;

  int num;
  if (a == p && gh_number_p (prev_acc))
    num = 0;
  else if ( (abs (a)<abs (p) || p*a<0) && a != 0 )
    num = 2;
  else
    num = 1;
  
  return a == p ? num : -num;
}

static int
number_accidentals (Music * note, Pitch *pitch, Translator_group * origin, 
		    SCM accidentals, SCM curbarnum)
{
  int number = 0;

  bool diff = false;
  if (gh_pair_p (accidentals) && !gh_symbol_p (ly_car (accidentals)))
    warning (_f ("Accidental typesetting list must begin with context-name: %s", 
		 ly_scm2string (ly_car (accidentals)).to_str0 ()));
  
  for (; gh_pair_p (accidentals) && origin; accidentals = gh_cdr (accidentals))
    {
      // If pair then it is a new accidentals typesetting rule to be checked
      SCM rule = gh_car (accidentals);
      if (gh_pair_p (rule))
	{
	  SCM type = gh_car (rule);
	  SCM lazyness = gh_cdr (rule);
	  SCM localsig = origin->get_property ("localKeySignature");
	  
	  bool same_octave_b = 
	    gh_eq_p (ly_symbol2scm ("same-octave"), type);
	  bool any_octave_b = 
	    gh_eq_p (ly_symbol2scm ("any-octave"), type);

	  if (same_octave_b || any_octave_b)
	    {
	      int n = number_accidentals_from_sig
		(localsig, note, pitch, curbarnum, lazyness, any_octave_b);
	      diff = diff || (n < 0);
	      number = max (number, abs (n));     
	    }
	  else
	    warning (_f ("unknown accidental typesetting: %s. Ignored", 
			 ly_symbol2string (type).to_str0 ()));
	}
      

      /*
	if symbol then it is a context name. Scan parent contexts to find it.
      */
      else if (gh_symbol_p (rule))
	{
	  Translator_group * dad = origin;
	  while (dad && !dad->is_alias (rule))
	    dad = dad->daddy_trans_;
      
	  if (dad)
	    origin = dad;
	}
      else warning (_f ("Accidental rule must be pair or context-name; Found %s", 
			ly_scm2string (rule).to_str0 ()));
    }

  return diff ? -number : number;
}

void
Accidental_engraver::process_acknowledged_grobs ()
{
  if (accidentals_.size () && !accidentals_.top().done_)
    {
      //SCM localsig = get_property ("localKeySignature");
      SCM accidentals =  get_property ("autoAccidentals");
      SCM cautionaries =  get_property ("autoCautionaries");
      SCM barnum = get_property ("currentBarNumber");
      SCM smp = get_property("measurePosition");
      Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
      if(mp.main_part_<Rational(0) && gh_number_p(barnum)) barnum = scm_int2num(gh_scm2int(barnum)-1);
      bool extra_natural_b = get_property ("extraNatural") == SCM_BOOL_T;
      for (int i = 0; i  < accidentals_.size (); i++) 
	{
	  if (accidentals_[i].done_ )
	    continue;
	  accidentals_[i].done_  = true;
	  Grob * support = accidentals_[i].head_;
	  Music * note = accidentals_[i].melodic_;
	  Translator_group * origin = accidentals_[i].origin_;

	  Pitch * pitch = unsmob_pitch (note->get_mus_property ("pitch"));
	  if (!pitch)
	    continue;
	  
	  int num = number_accidentals (note, pitch, origin, accidentals, barnum);
	  int num_caut = number_accidentals (note, pitch, origin, cautionaries, barnum);
	  bool cautionary = to_boolean (note->get_mus_property ("cautionary"));
	  
	  if (abs (num_caut) > abs (num))
	    {
	      num = num_caut;
	      cautionary = true;
	    }

	  if(num==0 && to_boolean (note->get_mus_property ("force-accidental")))
	     num=1;
	  
	  bool different = num < 0;
	  num = abs (num);

	  /* see if there's a tie that "changes" the accidental */
	  /* works because if there's a tie, the note to the left
	     is of the same pitch as the actual note */

	  Grob *tie_break_reminder = 0;
	  bool tie_changes = false;
	  for (int j = 0; j < ties_.size (); j++)
	    if (support == Tie::head (ties_[j], RIGHT))
	      {
		tie_changes = different;

		/* Enable accidentals for broken tie

		We only want an accidental on a broken tie, 
		if the tie changes the accidental.
		   
		Maybe check property noTieBreakForceAccidental? */
		if (different)
		  tie_break_reminder = ties_[j];
		break;
	      }

	  if (num)
	    {
	      Grob * a = make_item ("Accidental");
	      a->set_parent (support, Y_AXIS);

	      if (!accidental_placement_)
		{
		  accidental_placement_ = make_item ("AccidentalPlacement");
		  announce_grob (accidental_placement_, a->self_scm());
		}
	      
	      Accidental_placement::add_accidental (accidental_placement_, a);
	      announce_grob (a, SCM_EOL);

	      
	      SCM accs = gh_cons (scm_int2num (pitch->get_alteration ()), SCM_EOL);
	      if (num == 2 && extra_natural_b)
		accs = gh_cons (scm_int2num (0), accs);

	      /* TODO:

	      add cautionary option in accidental.
	       */

	      if (cautionary)
		{
		  a->set_grob_property ("cautionary", SCM_BOOL_T);
		}
	      
	      if (tie_break_reminder)
		{
		  // TODO.
		  a->set_grob_property ("tie", tie_break_reminder->self_scm());
		}
	      
	      
	      support->set_grob_property ("accidental-grob", a->self_scm ());

	      a->set_grob_property ("accidentals", accs);
	      accidentals_[i].accidental_ = a;
 /*
	We add the accidentals to the support of the arpeggio, so it is put left of the
	accidentals. 
	
      */
	      for (int i = 0;  i < left_objects_.size ();  i++)
		Side_position_interface::add_support (left_objects_[i], a);
	      for (int i = 0;  i < right_objects_.size ();  i++)
		Side_position_interface::add_support (a, right_objects_[i]);
	    }
	  

	  /*
	    We should not record the accidental if it is the first
	    note and it is tied from the previous measure.

	    Checking whether it is tied also works mostly, but will it
	    always do the correct thing?
	  */
	  

	  int n = pitch->get_notename ();
	  int o = pitch->get_octave ();
	  int a = pitch->get_alteration ();
	  SCM on_s = gh_cons (scm_int2num (o), scm_int2num (n));

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

	  while (origin)
	    {
	      SCM localsig = origin->get_property ("localKeySignature");
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
		    (localsig, on_s, gh_cons (scm_int2num (a), barnum)); 
		}
	      origin->set_property ("localKeySignature",  localsig);
	      origin = origin->daddy_trans_;
	    }
	}
    }
}

void
Accidental_engraver::finalize ()
{
  last_keysig_ = SCM_EOL;
}

void
Accidental_engraver::stop_translation_timestep ()
{
  for (int i = 0; i < accidentals_.size(); i++)
    {
      Grob *a = accidentals_[i].accidental_;
      if (a)
	{
	  typeset_grob (a);
	}
    }

  if (accidental_placement_)
    typeset_grob(accidental_placement_);
  accidental_placement_ = 00;
  
  accidentals_.clear();
  left_objects_.clear ();
  right_objects_.clear ();
  ties_.clear ();
}

void
Accidental_engraver::acknowledge_grob (Grob_info info)
{
  Music * note =  info.music_cause ();

  if (note
      && note->is_mus_type("note-event")
      && Rhythmic_head::has_interface (info.grob_))
    {
      Accidental_entry entry ;
      entry.head_ = info.grob_;
      entry.origin_ = info.origin_trans_->daddy_trans_;
      entry.melodic_ = note;

      accidentals_.push (entry);
    }
  else if (Tie::has_interface (info.grob_))
    {
      ties_.push (info.grob_);
    }
  else if (Arpeggio::has_interface (info.grob_))
    {
      left_objects_.push (info.grob_); 
    }
  else if (info.grob_->internal_has_interface (ly_symbol2scm("finger-interface")))
    {
      left_objects_.push (info.grob_); 
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
      Translator_group * trans_ = daddy_trans_;
      while (trans_)
	{
	  trans_ -> set_property ("localKeySignature",  ly_deep_copy (sig));
	  trans_ = trans_->daddy_trans_;
	}
      set_property_on_children(daddy_trans_,"localKeySignature", sig);

      last_keysig_ = sig;
    }
}





ENTER_DESCRIPTION (Accidental_engraver,
"Make accidentals.  Catches note heads, ties and notices key-change "
" events.  Due to interaction with ties (which don't come together "
" with note heads), this needs to be in a context higher than Tie_engraver.",
	       "Accidental",
/* accepts */     "",
	       "finger-interface rhythmic-head-interface tie-interface arpeggio-interface",
	       "localKeySignature extraNatural autoAccidentals autoCautionaries",
		   "localKeySignature");
