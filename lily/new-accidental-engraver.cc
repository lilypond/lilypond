/*
  new-accidental-engraver.cc -- implement new_accidental_engraver

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Modified 2001--2002 by Rune Zedeler <rz@daimi.au.dk>

  This is an experimental file - producing correct accidentals but
  unfortunately ruining the spacing. -rz
*/

#include "musical-request.hh"
#include "command-request.hh"
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

/**


FIXME: should not compute vertical positioning of accidentals, but
get them from the noteheads

The algorithm for accidentals should be documented, and made
tweakable.

*/

struct New_accidental_entry {
  int pass_done_;
  int number_accidentals_;
  int number_cautionaries_;
  bool different_;
  Note_req * melodic_;
  Grob * accidental_;
  Translator_group *origin_;
  Grob*  head_;
  New_accidental_entry();
};

New_accidental_entry::New_accidental_entry()
{
  pass_done_ = 0;
  number_accidentals_ = 0;
  number_cautionaries_ = 0;
  different_ = false;
  melodic_ = 0;
  accidental_ = 0;
  origin_ = 0;
  head_ = 0;
}

struct New_accidental_engraver : Engraver {
protected:
  TRANSLATOR_DECLARATIONS (New_accidental_engraver);
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void initialize ();
  virtual void process_acknowledged_grobs ();
  virtual void finalize ();
  virtual void process_grobs_first_pass ();
  virtual void process_grobs_second_pass ();

  public:

  /*
    TODO -> property.
    
    This is not a property, and it is not protected.  This poses a
    very small risk of the value being GC'd from under us.
  */
  SCM last_keysig_;

  /*
    Urgh. Since the accidentals depend on lots of variables, we have to
    store all information before we can really create the accidentals.
  */
  Link_array<Grob> arpeggios_;

  Grob * accidental_placement_;
  

  /*
    The next 
   */
  Array<New_accidental_entry> accidentals_;
  
  Link_array<Grob> ties_;


};


New_accidental_engraver::New_accidental_engraver ()
{
  accidental_placement_ = 0;
  last_keysig_ = SCM_EOL;
}

/* inserts the source alist into the destination alist, erasing old entries.
   result is: dest = merged
*/ 
static SCM merge_alists_front_x (SCM src, SCM dest) {
  if(gh_pair_p(src)) {
    dest = merge_alists_front_x(ly_cdr(src),dest);
    dest = ly_assoc_front_x(dest, ly_caar(src), ly_cdar(src));
  }
  return dest;
}

static void merge_property_on_children (Translator_group * trans,
				       const char * from_sym, const char * to_sym)
{
  SCM from = trans->get_property(from_sym);
  SCM to = trans->get_property(to_sym);
  to = merge_alists_front_x(from, to);
  trans->set_property (to_sym,  to);
  trans->set_property (from_sym, SCM_EOL);
  for (SCM p = trans -> trans_group_list_; gh_pair_p (p); p = ly_cdr(p)) {
    Translator_group *trg =  dynamic_cast<Translator_group*> (unsmob_translator (ly_car (p)));
    merge_property_on_children(trg, from_sym, to_sym);
  }
}

static void merge_property_on_family (Translator_group * trans,
				      const char * from_sym, const char * to_sym)
{
  merge_property_on_children (trans, from_sym, to_sym);
  trans = trans->daddy_trans_;
  while (trans)
    {
      SCM from = trans->get_property(from_sym);
      SCM to = trans->get_property(to_sym);
      to = merge_alists_front_x(from, to);
      trans->set_property (to_sym,  to);
      trans->set_property (from_sym, SCM_EOL);
      trans = trans->daddy_trans_;
    }
}

static void set_property_on_children (Translator_group * trans, const char * sym, SCM val)
{
  trans->set_property (sym, val);
  for (SCM p = trans -> trans_group_list_; gh_pair_p (p); p = ly_cdr(p)) {
    Translator_group *trg =  dynamic_cast<Translator_group*> (unsmob_translator (ly_car (p)));
    set_property_on_children(trg,sym,ly_deep_copy(val));
  }
}

static void set_property_on_family(Translator_group * trans, const char * sym, SCM val)
{
  set_property_on_children (trans, sym, val);
  trans = trans->daddy_trans_;
  while (trans)
    {
      trans -> set_property (sym,  ly_deep_copy (val));
      trans = trans->daddy_trans_;
    }
}

void
New_accidental_engraver::initialize ()
{
  // to ensure that process_music will initialize last_keysig_
  last_keysig_ = SCM_BOOL_F;
}

/*
calculates the number of accidentals on basis of the current local key sig
  (passed as argument)
  Returns number of accidentals (0, 1 or 2).
    Negative (-1 or -2) if accidental has changed.

*/
static int
number_accidentals (SCM sig, Note_req * note, Pitch *pitch, SCM curbarnum, SCM lazyness, 
		    bool ignore_octave_b)
{
  int n = pitch->notename_;
  int o = pitch->octave_;
  int a = pitch->alteration_;
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
      && !to_boolean (note->get_mus_property ("force-accidental"))
      && gh_number_p (prev_acc))
    num = 0;
  else if ( (abs (a)<abs (p) || p*a<0) && a != 0 )
    num = 2;
  else
    num = 1;
  
  return a == p ? num : -num;
}

static int
number_accidentals (Note_req * note, Pitch *pitch, Translator_group * origin, 
		    SCM accidentals, SCM curbarnum)
{
  int number = 0;

  bool diff = false;
  if (gh_pair_p (accidentals) && !gh_symbol_p (ly_car (accidentals)))
    warning (_f ("Accidental typesetting list must begin with context-name: %s", 
		 ly_scm2string (ly_car (accidentals)).to_str0 ()));
  
  while (gh_pair_p (accidentals) && origin)
    {
      // If pair then it is a new accidentals typesetting rule to be checked
      if (gh_pair_p (ly_car (accidentals)))
	{
	  SCM type = gh_caar (accidentals);
	  SCM lazyness = gh_cdar (accidentals);
	  SCM localsig = origin->get_property ("localKeySignature");
	  
	  bool same_octave_b = 
	    gh_eq_p (ly_symbol2scm ("same-octave"), type);
	  bool any_octave_b = 
	    gh_eq_p (ly_symbol2scm ("any-octave"), type);

	  if (same_octave_b || any_octave_b)
	    {
	      int n = number_accidentals
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
      else if (gh_symbol_p (ly_car (accidentals)))
	{
	  String context = ly_symbol2string (ly_car (accidentals));
	  
	  while (origin && !origin->is_alias_b (context))
	    origin = origin->daddy_trans_;
      
	  if (!origin)
	    warning (_f ("Symbol is not a parent context: %s. Ignored", 
			 context.to_str0 ()));
	}
      else warning (_f ("Accidental typesetting must be pair or context-name: %s", 
			ly_scm2string (ly_car (accidentals)).to_str0 ()));
      
      accidentals = ly_cdr (accidentals);
    }
  return diff ? -number : number;
}


/* 
  Perhaps one should join the two functions into one function taking an
  argument (pass).
  OTOH even though code would be smaller, spaghetti-level would increase.
*/
void
New_accidental_engraver::process_grobs_first_pass ()
{
  SCM accidentals =  get_property ("autoAccidentals");
  SCM cautionaries =  get_property ("autoCautionaries");
  SCM barnum = get_property ("currentBarNumber");

  for (int i = 0; i  < accidentals_.size (); i++) 
    {
      if (accidentals_[i].pass_done_ >= 1)
	continue;
      accidentals_[i].pass_done_  = 1;

      Grob * support = accidentals_[i].head_;
      Note_req * note = accidentals_[i].melodic_;
      Translator_group * origin = accidentals_[i].origin_;
      Pitch * pitch = unsmob_pitch (note->get_mus_property ("pitch"));

      int num;
      num = number_accidentals (note, pitch, origin, accidentals, barnum);
      accidentals_[i].number_accidentals_ = abs(num);
      accidentals_[i].different_ = num<0;

      num = number_accidentals (note, pitch, origin, cautionaries, barnum);
      accidentals_[i].number_cautionaries_ = abs(num);
      accidentals_[i].different_ = accidentals_[i].different_ || num<0;

      bool tie_changes = false;
      for (int j = 0; j < ties_.size (); j++)
	if (support == Tie::head (ties_[j], RIGHT))
	  tie_changes = accidentals_[i].different_;
      int n = pitch->notename_;
      int o = pitch->octave_;
      int a = pitch->alteration_;
      SCM o_s = gh_int2scm (o);
      SCM n_s = gh_int2scm (n);
      SCM on_s = gh_cons (o_s,n_s);
      
      while (origin)
	{
	  SCM sigch = origin->get_property ("localKeySignatureChanges");
	  SCM alt;
	  if (tie_changes)
	    /*
	      Remember an alteration that is different both from
	      that of the tied note and of the key signature.
	    */
	    alt = SCM_BOOL_T;
	  else
	    alt = gh_int2scm (a);
	  bool other_alt_same_oct = false;
	  bool other_alt_any_oct = false;
	  for (SCM j = sigch; gh_pair_p(j); j = ly_cdr(j)) {
	    SCM entry = ly_car(j);
	    /* if same notename has a different alt already recorded: */
	    if(gh_equal_p(ly_cdar(entry),n_s) && !gh_equal_p(ly_cadr(entry),alt))
	      {
		/* if it is also in same octave */
		if(gh_equal_p(ly_caar(entry),o_s))
		  other_alt_same_oct = true;
		else
		  other_alt_any_oct = true;
	      }
	  }
	  if(other_alt_same_oct)
	    alt = SCM_BOOL_T;
	  sigch = ly_assoc_front_x (sigch, on_s, gh_cons(alt,barnum)); 
	  if(other_alt_any_oct && !other_alt_same_oct) {
	    sigch = ly_assoc_front_x (sigch, on_s, gh_cons(SCM_BOOL_T,barnum));
	  }
	  origin->set_property ("localKeySignatureChanges",  sigch);
	  origin = origin->daddy_trans_;  
	}
    }
}

void
New_accidental_engraver::process_grobs_second_pass ()
{
  SCM accidentals =  get_property ("autoAccidentals");
  SCM cautionaries =  get_property ("autoCautionaries");
  SCM barnum = get_property ("currentBarNumber");
  
  bool extra_natural_b = get_property ("extraNatural") == SCM_BOOL_T;
  for (int i = 0; i  < accidentals_.size (); i++) 
    {
      if (accidentals_[i].pass_done_ >= 2)
	continue;
      accidentals_[i].pass_done_  = 2;
      Grob * support = accidentals_[i].head_;
      Note_req * note = accidentals_[i].melodic_;
      Translator_group * origin = accidentals_[i].origin_;
      
      Pitch * pitch = unsmob_pitch (note->get_mus_property ("pitch"));

      int num;
      num = number_accidentals (note, pitch, origin, accidentals, barnum);
      accidentals_[i].number_accidentals_ =
	max (accidentals_[i].number_accidentals_, abs(num));
      accidentals_[i].different_ = accidentals_[i].different_ || num<0;

      num = number_accidentals (note, pitch, origin, cautionaries, barnum);
      accidentals_[i].number_cautionaries_ =
	max (accidentals_[i].number_cautionaries_, abs(num));
      accidentals_[i].different_ = accidentals_[i].different_ || num<0;


      bool cautionary = to_boolean (note->get_mus_property ("cautionary"));
      
      if (accidentals_[i].number_cautionaries_ >accidentals_[i].number_accidentals_ )
	{
	  num = accidentals_[i].number_cautionaries_;
	  cautionary = true;
	}
      else
	  num = accidentals_[i].number_accidentals_;

      bool tie_changes = false;
      Grob *tie_break_reminder = 0;
      for (int j = 0; j < ties_.size (); j++)
	if (support == Tie::head (ties_[j], RIGHT))
	  {
	    tie_changes = accidentals_[i].different_;
	    tie_break_reminder = ties_[j];
	  }
      
      if (num)
	{
	  Grob * a = new Item (get_property ("Accidental"));
	  a->set_parent (support, Y_AXIS);
	  
	  if (!accidental_placement_)
	    {
	      accidental_placement_ = new Item (get_property ("AccidentalPlacement"));
	      announce_grob (accidental_placement_, a->self_scm());
	    }
	  
	  Accidental_placement::add_accidental (accidental_placement_, a);
	  announce_grob (a, SCM_EOL);
	  
	  
	  SCM accs = gh_cons (gh_int2scm (pitch->alteration_), SCM_EOL);
	  if (num == 2 && extra_natural_b)
	    accs = gh_cons (gh_int2scm (0), accs);
	  
	  if (cautionary)
	    {
	      a->set_grob_property ("cautionary", SCM_BOOL_T);
	    }
	  
	  if (tie_break_reminder)
	    {
	      a->set_grob_property ("tie", tie_break_reminder->self_scm());
	    }
	  
	  
	  support->set_grob_property ("accidental-grob", a->self_scm ());
	  
	  a->set_grob_property ("accidentals", accs);
	  accidentals_[i].accidental_ = a;
	  /*
	    We add the accidentals to the support of the arpeggio, so it is
	    put left of the accidentals. 
	  */
	  for (int i = 0;  i < arpeggios_.size ();  i++)
	    Side_position_interface::add_support (arpeggios_[i], a);
	}       
    }
}

void
New_accidental_engraver::process_acknowledged_grobs ()
{
  if (accidentals_.size () && accidentals_.top().pass_done_ < 1)
    process_grobs_first_pass ();
}

void
New_accidental_engraver::finalize ()
{

}

void
New_accidental_engraver::stop_translation_timestep ()
{
  merge_property_on_family(daddy_trans_, "localKeySignatureChanges", "localKeySignature");
  if (accidentals_.size () && accidentals_.top().pass_done_ < 2)
    process_grobs_second_pass ();

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

  set_property_on_family(daddy_trans_, "localKeySignatureChanges", SCM_EOL);  
  accidentals_.clear();
  arpeggios_.clear ();
  ties_.clear ();
}

void
New_accidental_engraver::acknowledge_grob (Grob_info info)
{
  Note_req * note =  dynamic_cast <Note_req *> (info.music_cause ());

  if (note && Rhythmic_head::has_interface (info.grob_))
    {
      New_accidental_entry entry ;
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
      arpeggios_.push (info.grob_); 
    }
  
}

void
New_accidental_engraver::process_music ()
{
  SCM sig = get_property ("keySignature");

  /* Detect key sig changes.
     Update all parents and children
  */
  if (last_keysig_ != sig)
    {
      set_property_on_family(daddy_trans_, "localKeySignature", sig);
      set_property_on_family(daddy_trans_, "localKeySignatureChanges", SCM_EOL); //This souldn't be neccesary
      last_keysig_ = sig;
    }
}





ENTER_DESCRIPTION (New_accidental_engraver,
"Make accidentals.  Catches note heads, ties and notices key-change
events.  Due to interaction with ties (which don't come together
with note heads), this needs to be in a context higher than Tie_engraver.",
		   
	       "Accidental",
	       "rhythmic-head-interface tie-interface arpeggio-interface",
	       "localKeySignature localKeySignatureChanges extraNatural autoAccidentals autoCautionaries",
		   "localKeySignature localKeySignatureChanges");
