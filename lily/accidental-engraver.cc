/*
  accidental-engraver.cc -- implement accidental_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Modified 2001--2002 by Rune Zedeler <rz@daimi.au.dk>
*/

#include "accidental-placement.hh"
#include "arpeggio.hh"
#include "context.hh"
#include "engraver.hh"
#include "international.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "stream-event.hh"
#include "tie.hh"
#include "warn.hh"

#include "translator.icc"

class Accidental_entry
{
public:
  bool done_;
  Stream_event *melodic_;
  Grob *accidental_;
  Context *origin_;
  Engraver *origin_engraver_;
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
  int get_bar_number ();
  void update_local_key_signature ();
  void create_accidental (Accidental_entry *entry, bool, bool);
  Grob *make_standard_accidental (Stream_event *note, Grob *note_head, Engraver *trans);
  Grob *make_suggested_accidental (Stream_event *note, Grob *note_head, Engraver *trans);

protected:
  TRANSLATOR_DECLARATIONS (Accidental_engraver);
  void process_music ();

  void acknowledge_tie (Grob_info);
  void acknowledge_arpeggio (Grob_info);
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_finger (Grob_info);

  void stop_translation_timestep ();
  virtual void initialize ();
  void process_acknowledged ();
  virtual void finalize ();
  virtual void derived_mark () const;

public:
  SCM last_keysig_;	// ugh.

  /*
    Urgh. Since the accidentals depend on lots of variables, we have
    to store all information before we can really create the
    accidentals.
  */
  vector<Grob*> left_objects_;
  vector<Grob*> right_objects_;

  Grob *accidental_placement_;

  vector<Accidental_entry> accidentals_;
  vector<Spanner*> ties_;
};

/*
  TODO:

  ugh, it is not clear what properties are mutable and which
  aren't. eg. localKeySignature is changed at runtime, which means
  that references in grobs should always store ly_deep_copy ()s of
  those.
*/


Accidental_engraver::Accidental_engraver ()
{
  accidental_placement_ = 0;
  last_keysig_ = SCM_EOL;
}

void
Accidental_engraver::derived_mark () const
{
  scm_gc_mark (last_keysig_);
}

void
Accidental_engraver::update_local_key_signature ()
{
  last_keysig_ = get_property ("keySignature");
  set_context_property_on_children (context (),
				    ly_symbol2scm ("localKeySignature"),
				    last_keysig_);

  Context *trans = context ()->get_parent_context ();

  /* Huh. Don't understand what this is good for. --hwn.  */

  while (trans)
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

static bool
recent_enough (int bar_number, SCM alteration_def, SCM laziness)
{
  if (scm_is_number (alteration_def)
      || laziness == SCM_BOOL_T)
    return true;

  return (bar_number <= scm_to_int (scm_cdr (alteration_def)) + scm_to_int (laziness));
}

static int
extract_alteration (SCM alteration_def)
{
  if (scm_is_number (alteration_def))
    return scm_to_int (alteration_def);
  else if (scm_is_pair (alteration_def))
    return scm_to_int (scm_car (alteration_def));
  else if (alteration_def == SCM_BOOL_F)
    return 0;
  else
    assert (0);
  return 0;
}

bool
is_tied (SCM alteration_def)
{
  return (alteration_def == SCM_BOOL_T)
    || (scm_is_pair (alteration_def) && scm_car (alteration_def) == SCM_BOOL_T);
}

static int
number_accidentals_from_sig (bool *different, SCM sig, Pitch *pitch,
			     int bar_number, SCM laziness, bool ignore_octave)
{
  int n = pitch->get_notename ();
  int o = pitch->get_octave ();

  SCM previous_alteration = SCM_BOOL_F;

  SCM from_same_octave = ly_assoc_get (scm_cons (scm_from_int (o),
						 scm_from_int (n)), sig, SCM_BOOL_F);
  SCM from_key_signature = ly_assoc_get (scm_from_int (n), sig, SCM_BOOL_F);
  SCM from_other_octaves = SCM_BOOL_F;
  for (SCM s = sig; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      if (scm_is_pair (scm_car (entry))
	  && scm_cdar (entry) == scm_from_int (n))
	{
	  from_other_octaves = scm_cdr (entry);
	  break;
	}
    }

  if (!ignore_octave
      && from_same_octave != SCM_BOOL_F
      && recent_enough (bar_number, from_same_octave, laziness))
    previous_alteration = from_same_octave;
  else if (ignore_octave
	   && from_other_octaves != SCM_BOOL_F
	   && recent_enough (bar_number, from_other_octaves, laziness))
    previous_alteration = from_other_octaves;
  else if (from_key_signature != SCM_BOOL_F)
    previous_alteration = from_key_signature;

  int num = 1;
  if (is_tied (previous_alteration))
    {
      num = 1;
      *different = true;
    }
  else
    {
      int prev = extract_alteration (previous_alteration);
      int alter = pitch->get_alteration ();

      if (alter == prev)
	num = 0;
      else if ((abs (alter) < abs (prev)
		|| prev * alter < 0) && alter != 0)
	num = 2;
      *different = (alter != prev);
    }
  return num;
}

static int
number_accidentals (bool *different,
		    Pitch *pitch, Context *origin,
		    SCM accidentals, int bar_number)
{
  int number = 0;

  *different = false;
  if (scm_is_pair (accidentals) && !scm_is_symbol (scm_car (accidentals)))
    warning (_f ("accidental typesetting list must begin with context-name: %s",
		 ly_scm2string (scm_car (accidentals)).c_str ()));

  for (; scm_is_pair (accidentals) && origin;
       accidentals = scm_cdr (accidentals))
    {
      // If pair then it is a new accidentals typesetting rule to be checked
      SCM rule = scm_car (accidentals);
      if (scm_is_pair (rule))
	{
	  SCM type = scm_car (rule);
	  SCM laziness = scm_cdr (rule);
	  SCM localsig = origin->get_property ("localKeySignature");

	  bool same_octave_b
	    = scm_is_eq (ly_symbol2scm ("same-octave"), type);
	  bool any_octave_b
	    = scm_is_eq (ly_symbol2scm ("any-octave"), type);

	  if (same_octave_b || any_octave_b)
	    {
	      bool d = false;
	      int n = number_accidentals_from_sig
		(&d, localsig, pitch, bar_number, laziness, any_octave_b);
	      *different = *different || d;
	      number = max (number, n);
	    }
	  else
	    warning (_f ("ignoring unknown accidental: %s",
			 ly_symbol2string (type).c_str ()));
	}

      /* if symbol then it is a context name.  Scan parent contexts to
	 find it. */
      else if (scm_is_symbol (rule))
	{
	  Context *dad = origin;
	  while (dad && !dad->is_alias (rule))
	    dad = dad->get_parent_context ();

	  if (dad)
	    origin = dad;
	}
      else
	warning (_f ("pair or context-name expected for accidental rule, found %s",
		     ly_scm2string (rule).c_str ()));
    }

  return number;
}

int
Accidental_engraver::get_bar_number ()
{
  SCM barnum = get_property ("internalBarNumber");
  SCM smp = get_property ("measurePosition");

  int bn = robust_scm2int (barnum, 0);

  Moment mp = robust_scm2moment (smp, Moment (0));
  if (mp.main_part_ < Rational (0))
    bn--;

  return bn;
}

void
Accidental_engraver::process_acknowledged ()
{
  if (accidentals_.size () && !accidentals_.back ().done_)
    {
      SCM accidentals = get_property ("autoAccidentals");
      SCM cautionaries = get_property ("autoCautionaries");
      int barnum = get_bar_number ();

      for (vsize i = 0; i < accidentals_.size (); i++)
	{
	  if (accidentals_[i].done_)
	    continue;
	  accidentals_[i].done_ = true;

	  Stream_event *note = accidentals_[i].melodic_;
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

	  bool forced = to_boolean (note->get_property ("force-accidental"));
	  if (num == 0 && forced)
	    num = 1;

	  /* Cannot look for ties: it's not guaranteed that they reach
	     us before the notes. */
	  if (num
	      && !note->in_event_class ("trill-span-event"))
	    create_accidental (&accidentals_[i], num > 1, cautionary);


	  if (forced || cautionary)
	    accidentals_[i].accidental_->set_property ("forced", SCM_BOOL_T);
	}
    }
}

void
Accidental_engraver::create_accidental (Accidental_entry *entry,
					bool restore_natural,
					bool cautionary)
{
  Stream_event *note = entry->melodic_;
  Grob *support = entry->head_;
  Pitch *pitch = unsmob_pitch (note->get_property ("pitch"));

  bool as_suggestion = to_boolean (entry->origin_->get_property ("suggestAccidentals"));
  Grob *a = 0;
  if (as_suggestion)
    a = make_suggested_accidental (note, support, entry->origin_engraver_);
  else
    a = make_standard_accidental (note, support, entry->origin_engraver_);

  SCM accs = scm_cons (scm_from_int (pitch->get_alteration ()),
		       SCM_EOL);
  if (restore_natural)
    {
      if (to_boolean (get_property ("extraNatural")))
	accs = scm_cons (scm_from_int (0), accs);
    }

  /* TODO: add cautionary option in accidental. */
  if (cautionary)
    a->set_property ("cautionary", SCM_BOOL_T);

  a->set_property ("accidentals", accs);
  entry->accidental_ = a;
}

Grob *
Accidental_engraver::make_standard_accidental (Stream_event *note,
					       Grob *note_head,
					       Engraver *trans)
{

  (void)note;
  /*
    We construct the accidentals at the originating Voice
    level, so that we get the property settings for
    Accidental from the respective Voice.
  */
  Grob *a = trans->make_item ("Accidental", note_head->self_scm ());

  /*
    We add the accidentals to the support of the arpeggio,
    so it is put left of the accidentals.
  */
  for (vsize i = 0; i < left_objects_.size (); i++)
    {
      if (left_objects_[i]->get_property ("side-axis") == scm_from_int (X_AXIS))
	Side_position_interface::add_support (left_objects_[i], a);
    }

  /*
    Hmm. Junkme? 
   */
  for (vsize i = 0; i < right_objects_.size (); i++)
    Side_position_interface::add_support (a, right_objects_[i]);

  a->set_parent (note_head, Y_AXIS);

  if (!accidental_placement_)
    accidental_placement_ = make_item ("AccidentalPlacement",
				       a->self_scm ());
  Accidental_placement::add_accidental (accidental_placement_, a);

  note_head->set_object ("accidental-grob", a->self_scm ());

  return a;
}

Grob *
Accidental_engraver::make_suggested_accidental (Stream_event *note,
						Grob *note_head,
						Engraver *trans)
{
  (void) note;
  Grob *a = trans->make_item ("AccidentalSuggestion", note_head->self_scm ());

  Side_position_interface::add_support (a, note_head);
  if (Grob *stem = unsmob_grob (a->get_object ("stem")))
    Side_position_interface::add_support (a, stem);

  a->set_parent (note_head, X_AXIS);
  return a;
}

void
Accidental_engraver::finalize ()
{
  last_keysig_ = SCM_EOL;
}

void
Accidental_engraver::stop_translation_timestep ()
{
  for (vsize j = ties_.size (); j--;)
    {
      Grob *r = Tie::head (ties_[j], RIGHT);
      for (vsize i = accidentals_.size (); i--;)
	if (accidentals_[i].head_ == r)
	  {
	    if (Grob *g = accidentals_[i].accidental_)
	      {
		g->set_object ("tie", ties_[j]->self_scm ());
		accidentals_[i].tied_ = true;
	      }
	    ties_.erase (ties_.begin () + j);
	    break;
	  }
    }

  for (vsize i = accidentals_.size (); i--;)
    {
      int barnum = get_bar_number ();

      Stream_event *note = accidentals_[i].melodic_;
      Context *origin = accidentals_[i].origin_;

      Pitch *pitch = unsmob_pitch (note->get_property ("pitch"));
      if (!pitch)
	continue;

      int n = pitch->get_notename ();
      int o = pitch->get_octave ();
      int a = pitch->get_alteration ();
      SCM key = scm_cons (scm_from_int (o), scm_from_int (n));

      SCM localsig = SCM_EOL;
      while (origin
	     && origin->where_defined (ly_symbol2scm ("localKeySignature"), &localsig))
	{
	  bool change = false;
	  if (accidentals_[i].tied_)
	    {
	      /*
		Remember an alteration that is different both from
		that of the tied note and of the key signature.
	      */
	      localsig = ly_assoc_front_x
		(localsig, key, scm_cons (SCM_BOOL_T, scm_from_int (barnum)));

	      change = true;
	    }
	  else
	    {
	      /*
		not really really correct if there are more than one
		noteheads with the same notename.
	      */
	      localsig = ly_assoc_front_x (localsig, key,
					   scm_cons (scm_from_int (a),
						     scm_from_int (barnum)));
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
Accidental_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  Stream_event *note = info.event_cause ();
  if (note
      && (note->in_event_class ("note-event")
	  || note->in_event_class ("trill-span-event")))
    {
      /*
	string harmonics usually don't have accidentals.
      */
      if (to_boolean (get_property ("harmonicAccidentals"))
	  || !ly_is_equal (info.grob ()->get_property ("style"),
			   ly_symbol2scm ("harmonic")))
	{
	  Accidental_entry entry;
	  entry.head_ = info.grob ();
	  entry.origin_engraver_ = dynamic_cast<Engraver *> (info.origin_translator ());
	  entry.origin_ = entry.origin_engraver_->context ();
	  entry.melodic_ = note;

	  accidentals_.push_back (entry);
	}
    }
}

void
Accidental_engraver::acknowledge_tie (Grob_info info)
{
  ties_.push_back (dynamic_cast<Spanner *> (info.grob ()));
}

void
Accidental_engraver::acknowledge_arpeggio (Grob_info info)
{
  left_objects_.push_back (info.grob ());
}

void
Accidental_engraver::acknowledge_finger (Grob_info info)
{
  left_objects_.push_back (info.grob ());
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

ADD_ACKNOWLEDGER (Accidental_engraver, arpeggio);
ADD_ACKNOWLEDGER (Accidental_engraver, finger);
ADD_ACKNOWLEDGER (Accidental_engraver, rhythmic_head);
ADD_ACKNOWLEDGER (Accidental_engraver, tie);

ADD_TRANSLATOR (Accidental_engraver,
		
		"Make accidentals.  "
		"Catch note heads, ties and notices key-change events.  "
		"This engraver usually lives at Staff level, but "
		"reads the settings for Accidental at @code{Voice} level, "
		"so you can @code{\\override} them at @code{Voice}. ",
		"Accidental AccidentalSuggestion",

		"autoAccidentals "
		"autoCautionaries "
		"internalBarNumber "
		"extraNatural "
		"harmonicAccidentals "
		"localKeySignature",
		"localKeySignature");
