/*
  accidental-engraver.cc -- implement accidental_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Modified 2001--2002 by Rune Zedeler <rz@daimi.au.dk>
*/

#include "accidental-placement.hh"
#include "arpeggio.hh"
#include "context.hh"
#include "engraver-group-engraver.hh"
#include "engraver.hh"
#include "event.hh"
#include "item.hh"
#include "protected-scm.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "tie.hh"
#include "warn.hh"


class Accidental_entry
{
public:
  bool done_;
  Music *melodic_;
  Grob *accidental_;
  Context *origin_;
  Grob *head_;
  bool tied_;

  Accidental_entry ();
};

Accidental_entry::Accidental_entry ()
{
  tied_ = false;
  done_ = false;
  melodic_ = 0;
  accidental_ = 0;
  origin_ = 0;
  head_ = 0;
}

class Accidental_engraver : public Engraver
{
public:
  int get_bar_number ();
  void update_local_key_signature ();

protected:
  TRANSLATOR_DECLARATIONS (Accidental_engraver);
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void initialize ();
  virtual void process_acknowledged_grobs ();
  virtual void finalize ();

public:
  Protected_scm last_keysig_;	// ugh.
  
  /* Urgh. Since the accidentals depend on lots of variables, we have
    to store all information before we can really create the
    accidentals.  */
  Link_array<Grob> left_objects_;
  Link_array<Grob> right_objects_;

  Grob *accidental_placement_;

  Array<Accidental_entry> accidentals_;
  Link_array<Spanner> ties_;
};


/*
  TODO:

  ugh, it is not clear what properties are mutable and which
  aren't. eg. localKeySignature is changed at runtime, which means
  that references in grobs should always store ly_deep_copy ()s of
  those.
 */

static void
set_property_on_children (Context *trans, char const *sym, SCM val)
{
  trans->set_property (sym, ly_deep_copy (val));
  for (SCM p = trans->children_contexts (); ly_c_pair_p (p); p = ly_cdr (p))
    {
      Context *trg = unsmob_context (ly_car (p));
      set_property_on_children (trg, sym, ly_deep_copy (val));
    }
}

Accidental_engraver::Accidental_engraver ()
{
  accidental_placement_ = 0;
  last_keysig_ = SCM_EOL;
}

void
Accidental_engraver::update_local_key_signature ()
{
  last_keysig_ = get_property ("keySignature");
  set_property_on_children (context (), "localKeySignature", last_keysig_);

  Context *trans = context ()->get_parent_context ();

  /* Huh. Don't understand what this is good for. --hwn.  */
  while (trans && trans->where_defined (ly_symbol2scm ("localKeySignature")))
    {
      trans->set_property ("localKeySignature", ly_deep_copy (last_keysig_));
      trans = trans->get_parent_context ();
    }
}

void
Accidental_engraver::initialize ()
{
  update_local_key_signature (); 
}


/** Calculate the number of accidentals on basis of the current local key
    sig (passed as argument)
    
    * First check step+octave (taking into account barnumbers if necessary).
   
    * Then check the global signature (only step).
  
    Return number of accidentals (0, 1 or 2).  */
static int
number_accidentals_from_sig (bool *different, SCM sig, Pitch *pitch,
			     int curbarnum, SCM laziness, bool ignore_octave)
{
  int n = pitch->get_notename ();
  int o = pitch->get_octave ();
  int a = pitch->get_alteration ();

  SCM prev_alt = SCM_BOOL_F;

  if (!ignore_octave)
    {
      SCM prev_local
	= scm_assoc (scm_cons (scm_int2num (o), scm_int2num (n)), sig);

      if (ly_c_pair_p (prev_local))
	{
	  if (ly_c_pair_p (ly_cdr (prev_local))
	      && ly_c_number_p (laziness))
	    {
	      int barnum = ly_scm2int (ly_cddr (prev_local));

	      prev_local = scm_cons (ly_car (prev_local), ly_cadr (prev_local));
	      if (curbarnum <= barnum + ly_scm2int (laziness))
		prev_alt = prev_local;
	    }
	}
    }

  if (prev_alt == SCM_BOOL_F)
    prev_alt = scm_assoc (scm_int2num (n), sig);

  prev_alt =  (prev_alt == SCM_BOOL_F) ? scm_int2num (0) : ly_cdr (prev_alt); 
    
  /* UGH. prev_acc can be #t in case of ties. What is this for?  */
  int p = ly_c_number_p (prev_alt) ? ly_scm2int (prev_alt) : 0;

  int num;
  if (a == p && ly_c_number_p (prev_alt))
    num = 0;
  else if ( (abs (a)<abs (p) || p*a<0) && a != 0 )
    num = 2;
  else
    num = 1;

  *different = (a != p);
  return num;
}

static int
number_accidentals (bool *different,
		    Pitch *pitch, Context *origin,
		    SCM accidentals, int curbarnum)
{
  int number = 0;

  *different = false;
  if (ly_c_pair_p (accidentals) && !ly_c_symbol_p (ly_car (accidentals)))
    warning (_f ("Accidental typesetting list must begin with context-name: %s", 
		 ly_scm2string (ly_car (accidentals)).to_str0 ()));
  
  for (; ly_c_pair_p (accidentals) && origin;
       accidentals = ly_cdr (accidentals))
    {
      // If pair then it is a new accidentals typesetting rule to be checked
      SCM rule = ly_car (accidentals);
      if (ly_c_pair_p (rule))
	{
	  SCM type = ly_car (rule);
	  SCM laziness = ly_cdr (rule);
	  SCM localsig = origin->get_property ("localKeySignature");
	  
	  bool same_octave_b = 
	    ly_c_eq_p (ly_symbol2scm ("same-octave"), type);
	  bool any_octave_b = 
	    ly_c_eq_p (ly_symbol2scm ("any-octave"), type);

	  if (same_octave_b || any_octave_b)
	    {
	      bool d = false;
	      int n = number_accidentals_from_sig
		(&d, localsig, pitch, curbarnum, laziness, any_octave_b);
	      *different = *different || d;
	      number = max (number, n);     
	    }
	  else
	    warning (_f ("ignoring unknown accidental: %s", 
			 ly_symbol2string (type).to_str0 ()));
	}
      

      /*
	if symbol then it is a context name. Scan parent contexts to find it.
      */
      else if (ly_c_symbol_p (rule))
	{
	  Context *dad = origin;
	  while (dad && !dad->is_alias (rule))
	    dad = dad->get_parent_context ();
      
	  if (dad)
	    origin = dad;
	}
      else warning (_f ("Accidental rule must be pair or context-name; Found %s", 
			ly_scm2string (rule).to_str0 ()));
    }

  return number;
}

int
Accidental_engraver::get_bar_number ()
{
  SCM barnum = get_property ("currentBarNumber");
  SCM smp = get_property ("measurePosition");

  int bn = robust_scm2int (barnum, 0);
  
  Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
  if (mp.main_part_ < Rational (0))
    bn--;
  
  return bn;
}

void
Accidental_engraver::process_acknowledged_grobs ()
{
  if (accidentals_.size () && !accidentals_.top ().done_)
    {
      SCM accidentals =  get_property ("autoAccidentals");
      SCM cautionaries =  get_property ("autoCautionaries");
      int barnum = get_bar_number ();
      
      bool extra_natural_b = get_property ("extraNatural") == SCM_BOOL_T;
      for (int i = 0; i < accidentals_.size (); i++) 
	{
	  if (accidentals_[i].done_ )
	    continue;
	  accidentals_[i].done_  = true;
	  Grob *support = accidentals_[i].head_;
	  Music *note = accidentals_[i].melodic_;
	  Context *origin = accidentals_[i].origin_;

	  Pitch *pitch = unsmob_pitch (note->get_property ("pitch"));
	  if (!pitch)
	    continue;

	  bool different = false;
	  bool different_caut = false;
	  
	  int num = number_accidentals (&different,
					pitch, origin,
					accidentals, barnum);
	  int num_caut = number_accidentals (&different_caut,
					     pitch, origin,
					     cautionaries, barnum);

	  bool cautionary = to_boolean (note->get_property ("cautionary"));
	  
	  if (num_caut > num)
	    {
	      num = num_caut;
	      different = different_caut;
	      cautionary = true;
	    }

	  if (num == 0 && to_boolean (note->get_property ("force-accidental")))
	    num = 1;
	  

	  /*
	    Can not look for ties: it's not guaranteed that they reach
	    us before the notes
	   */
	
	  if (num)
	    {
	      /*
		We construct the accidentals at the originating Voice
		level, so that we get the property settings for
		Accidental from the respective Voice.
	       */
	      Grob *a
		= make_item_from_properties (origin->implementation (),
					     ly_symbol2scm ("Accidental"),
					     note->self_scm ());
	      a->set_parent (support, Y_AXIS);

	      if (!accidental_placement_)
		accidental_placement_ = make_item ("AccidentalPlacement",
						   a->self_scm ());
	      Accidental_placement::add_accidental (accidental_placement_, a);
	      SCM accs = scm_cons (scm_int2num (pitch->get_alteration ()),
				   SCM_EOL);
	      if (num == 2 && extra_natural_b)
		accs = scm_cons (scm_int2num (0), accs);

	      /* TODO: add cautionary option in accidental. */

	      if (cautionary)
		a->set_property ("cautionary", SCM_BOOL_T);
	      
	      support->set_property ("accidental-grob", a->self_scm ());

	      a->set_property ("accidentals", accs);
	      accidentals_[i].accidental_ = a;

	      /*
		We add the accidentals to the support of the arpeggio,
		so it is put left of the accidentals.
	      */
	      for (int i = 0;  i < left_objects_.size ();  i++)
		Side_position_interface::add_support (left_objects_[i], a);
	      for (int i = 0;  i < right_objects_.size ();  i++)
		Side_position_interface::add_support (a, right_objects_[i]);
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
  for (int j = ties_.size (); j--;)
    {
      Grob *r = Tie::head (ties_[j], RIGHT);
      for (int i = accidentals_.size ();  i--;)
	if (accidentals_[i].head_ == r)
	  {
	    if (Grob *g = accidentals_[i].accidental_)
	      {
		g->set_property ("tie", ties_[j]->self_scm ());
		accidentals_[i].tied_ = true;
	      }
	    ties_.del (j);
	    break;
	  }
    }

  for (int i = accidentals_.size (); i--;) 
    {
      int barnum = get_bar_number ();

      Music *note = accidentals_[i].melodic_;
      Context * origin = accidentals_[i].origin_;

      Pitch *pitch = unsmob_pitch (note->get_property ("pitch"));
      if (!pitch)
	continue;

      int n = pitch->get_notename ();
      int o = pitch->get_octave ();
      int a = pitch->get_alteration ();
      SCM key = scm_cons (scm_int2num (o), scm_int2num (n));

      while (origin
	     && origin->where_defined (ly_symbol2scm ("localKeySignature")))
	{
	  /*
	    huh? we set props all the way to the top? 
	  */
	  SCM localsig = origin->get_property ("localKeySignature");
	  bool change = false;
	  if (accidentals_[i].tied_)
	    {
	      /*
		Remember an alteration that is different both from
		that of the tied note and of the key signature.
	      */
	      localsig = ly_assoc_front_x
		(localsig, key, scm_cons (SCM_BOOL_T, scm_int2num (barnum)));

	      change = true;
	    }
	  else
	    {
	      /*
		not really really correct if there are more than one
		noteheads with the same notename.
	      */
	      localsig = ly_assoc_front_x (localsig, key,
					   scm_cons (scm_int2num (a),
						     scm_int2num (barnum)));
	      change = true;
	    }

	  if (change)
	    origin->set_property ("localKeySignature", localsig);

	  origin = origin->get_parent_context ();
	}
    }

  accidental_placement_ = 0;
  accidentals_.clear ();
  left_objects_.clear ();
  right_objects_.clear ();
}

void
Accidental_engraver::acknowledge_grob (Grob_info info)
{
  Music *note = info.music_cause ();

  if (note
      && note->is_mus_type ("note-event")
      && Rhythmic_head::has_interface (info.grob_))
    {
      if (to_boolean ( get_property ("harmonicAccidentals"))
	  || !ly_c_equal_p (info.grob_->get_property ("style"),
			  ly_symbol2scm ("harmonic")))
	{
	  
	  Accidental_entry entry ;
	  entry.head_ = info.grob_;
	  entry.origin_ = info.origin_trans_->context ();
	  entry.melodic_ = note;

	  accidentals_.push (entry);
	}
    }
  else if (Tie::has_interface (info.grob_))
    ties_.push (dynamic_cast<Spanner*> (info.grob_));
  else if (Arpeggio::has_interface (info.grob_))
    left_objects_.push (info.grob_); 
  else if (info.grob_
	   ->internal_has_interface (ly_symbol2scm ("finger-interface")))
    left_objects_.push (info.grob_); 
}

void
Accidental_engraver::process_music ()
{
  SCM sig = get_property ("keySignature");
  /* Detect key sig changes.
     Update all parents and children.  */
  if (last_keysig_ != sig)
    update_local_key_signature ();
}

ENTER_DESCRIPTION (Accidental_engraver,
		   "Make accidentals.  "
		   "Catch note heads, ties and notices key-change events.  "
		   "This engraver usually lives at Staff level, but "
		   "reads the settings for Accidental at @code{Voice} level, " 
		   "so you can @code{\\override} them at @code{Voice}. "
		   ,
		   "Accidental"
		   ,
		   ""
		   ,
		   "arpeggio-interface "
		   "finger-interface "
		   "rhythmic-head-interface "
		   "tie-interface "
		   ,
		   "autoAccidentals "
		   "autoCautionaries "
		   "extraNatural "
		   "harmonicAccidentals "
		   "localKeySignature"
		   ,
		   "localKeySignature"
		   );
