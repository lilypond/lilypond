/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Modified 2001--2002 by Rune Zedeler <rz@daimi.au.dk>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "accidental-placement.hh"
#include "arpeggio.hh"
#include "context.hh"
#include "duration.hh"
#include "engraver.hh"
#include "international.hh"
#include "item.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "rhythmic-head.hh"
#include "separation-item.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "tie.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

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
  origin_engraver_ = 0;
  head_ = 0;
}

class Accidental_engraver : public Engraver
{
  void update_local_key_signature (SCM new_signature);
  void create_accidental (Accidental_entry *entry, bool, bool);
  Grob *make_standard_accidental (Stream_event *note, Grob *note_head,
                                  Engraver *trans, bool);
  Grob *make_suggested_accidental (Stream_event *note, Grob *note_head,
                                   Engraver *trans);

protected:
  TRANSLATOR_DECLARATIONS (Accidental_engraver);
  void process_music ();

  void acknowledge_end_tie (Grob_info);
  void acknowledge_arpeggio (Grob_info);
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_finger (Grob_info);
  void acknowledge_note_column (Grob_info);

  void stop_translation_timestep ();
  void process_acknowledged ();

  void finalize () override;
  void derived_mark () const override;

public:
  SCM last_keysig_;

  vector<Grob *> left_objects_;
  vector<Grob *> right_objects_;

  Grob *accidental_placement_;

  vector<Accidental_entry> accidentals_;
  vector<Spanner *> ties_;
  vector<Grob *> note_columns_;
};

/*
  localAlterations is changed at runtime, which means that references
  in grobs should always store ly_deep_copy ()s of those.
*/

Accidental_engraver::Accidental_engraver (Context *c) : Engraver (c)
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
Accidental_engraver::update_local_key_signature (SCM new_sig)
{
  last_keysig_ = new_sig;
  set_context_property_on_children (
      context (), ly_symbol2scm ("localAlterations"), new_sig);

  Context *trans = context ()->get_parent_context ();

  /*
    Reset parent contexts so that e.g. piano-accidentals won't remember old
    cross-staff accidentals after key-sig-changes.
  */

  SCM val;
  while (trans
         && trans->here_defined (ly_symbol2scm ("localAlterations"), &val))
    {
      trans->set_property ("localAlterations", ly_deep_copy (last_keysig_));
      trans = trans->get_parent_context ();
    }
}

struct Accidental_result
{
  bool need_acc;
  bool need_restore;

  Accidental_result () { need_restore = need_acc = false; }

  Accidental_result (bool restore, bool acc)
  {
    need_restore = restore;
    need_acc = acc;
  }

  Accidental_result (SCM scm)
  {
    need_restore = to_boolean (scm_car (scm));
    need_acc = to_boolean (scm_cdr (scm));
  }

  int score () const { return (need_acc ? 1 : 0) + (need_restore ? 1 : 0); }
};

static Accidental_result
check_pitch_against_rules (Pitch const &pitch, Context *origin, SCM rules,
                           int bar_number, SCM measurepos)
{
  Accidental_result result;
  SCM pitch_scm = pitch.smobbed_copy ();
  SCM barnum_scm = scm_from_int (bar_number);

  if (scm_is_pair (rules) && !scm_is_symbol (scm_car (rules)))
    warning (_f ("accidental typesetting list must begin with context-name: %s",
                 ly_scm2string (scm_car (rules)).c_str ()));

  for (; scm_is_pair (rules) && origin; rules = scm_cdr (rules))
    {
      SCM rule = scm_car (rules);
      if (ly_is_procedure (rule))
        {
          SCM rule_result_scm = scm_call_4 (rule, origin->self_scm (),
                                            pitch_scm, barnum_scm, measurepos);
          Accidental_result rule_result (rule_result_scm);

          result.need_acc |= rule_result.need_acc;
          result.need_restore |= rule_result.need_restore;
        }

      /*
        If symbol then it is a context name.  Scan parent contexts to
        find it.
      */
      else if (scm_is_symbol (rule))
        {
          Context *dad = find_context_above (origin, rule);
          if (dad)
            origin = dad;
        }
      else
        warning (_f (
            "procedure or context-name expected for accidental rule, found %s",
            print_scm_val (rule).c_str ()));
    }

  return result;
}

void
Accidental_engraver::process_acknowledged ()
{
  if (accidentals_.size () && !accidentals_.back ().done_)
    {
      SCM accidental_rules = get_property ("autoAccidentals");
      SCM cautionary_rules = get_property ("autoCautionaries");
      SCM measure_position = get_property ("measurePosition");
      int barnum = measure_number (context ());

      for (vsize i = 0; i < accidentals_.size (); i++)
        {
          if (accidentals_[i].done_)
            continue;
          accidentals_[i].done_ = true;

          Stream_event *note = accidentals_[i].melodic_;
          Context *origin = accidentals_[i].origin_;

          Pitch *pitch = unsmob<Pitch> (note->get_property ("pitch"));
          if (!pitch)
            continue;

          Accidental_result acc = check_pitch_against_rules (
              *pitch, origin, accidental_rules, barnum, measure_position);
          Accidental_result caut = check_pitch_against_rules (
              *pitch, origin, cautionary_rules, barnum, measure_position);

          bool cautionary = to_boolean (note->get_property ("cautionary"));
          if (caut.score () > acc.score ())
            {
              acc.need_acc |= caut.need_acc;
              acc.need_restore |= caut.need_restore;

              cautionary = true;
            }

          bool forced = to_boolean (note->get_property ("force-accidental"));
          if (!acc.need_acc && forced)
            acc.need_acc = true;

          /*
            Cannot look for ties: it's not guaranteed that they reach
            us before the notes.
          */
          if (!note->in_event_class ("trill-span-event"))
            {
              if (acc.need_acc)
                create_accidental (&accidentals_[i], acc.need_restore,
                                   cautionary);

              if (forced || cautionary)
                accidentals_[i].accidental_->set_property ("forced",
                                                           SCM_BOOL_T);
            }
        }
    }
}

void
Accidental_engraver::create_accidental (Accidental_entry *entry,
                                        bool restore_natural, bool cautionary)
{
  Stream_event *note = entry->melodic_;
  Grob *support = entry->head_;
  SCM suggest = entry->origin_->get_property ("suggestAccidentals");
  bool bsuggest = to_boolean (suggest);
  Grob *a = 0;
  if (bsuggest
      || (cautionary && scm_is_eq (suggest, ly_symbol2scm ("cautionary"))))
    a = make_suggested_accidental (note, support, entry->origin_engraver_);
  else
    a = make_standard_accidental (note, support, entry->origin_engraver_,
                                  cautionary);

  if (restore_natural)
    {
      if (to_boolean (get_property ("extraNatural")))
        a->set_property ("restore-first", SCM_BOOL_T);
    }

  entry->accidental_ = a;
}

Grob *
Accidental_engraver::make_standard_accidental (Stream_event * /* note */,
                                               Grob *note_head, Engraver *trans,
                                               bool cautionary)
{
  /*
    We construct the accidentals at the originating Voice
    level, so that we get the property settings for
    Accidental from the respective Voice.
  */
  Grob *a = 0;
  if (cautionary)
    a = trans->make_item ("AccidentalCautionary", note_head->self_scm ());
  else
    a = trans->make_item ("Accidental", note_head->self_scm ());

  /*
    We add the accidentals to the support of the arpeggio,
    so it is put left of the accidentals.
  */
  for (vsize i = 0; i < left_objects_.size (); i++)
    {
      if (ly_is_equal (left_objects_[i]->get_property ("side-axis"),
                       scm_from_int (X_AXIS)))
        Side_position_interface::add_support (left_objects_[i], a);
    }

  for (vsize i = 0; i < right_objects_.size (); i++)
    Side_position_interface::add_support (a, right_objects_[i]);

  a->set_parent (note_head, Y_AXIS);

  if (!accidental_placement_)
    accidental_placement_ = make_item ("AccidentalPlacement", a->self_scm ());

  Accidental_placement::add_accidental (
      accidental_placement_, a,
      scm_is_eq (get_property ("accidentalGrouping"), ly_symbol2scm ("voice")),
      (long)trans);

  note_head->set_object ("accidental-grob", a->self_scm ());

  return a;
}

Grob *
Accidental_engraver::make_suggested_accidental (Stream_event * /* note */,
                                                Grob *note_head,
                                                Engraver *trans)
{
  Grob *a = trans->make_item ("AccidentalSuggestion", note_head->self_scm ());

  Side_position_interface::add_support (a, note_head);
  if (Grob *stem = unsmob<Grob> (a->get_object ("stem")))
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
      Grob *l = Tie::head (ties_[j], LEFT);
      if (l && r)
        {
          // Don't mark accidentals as "tied" when the pitch is not
          // actually the same.  This is relevant for enharmonic ties.
          Stream_event *le = unsmob<Stream_event> (l->get_property ("cause"));
          Stream_event *re = unsmob<Stream_event> (r->get_property ("cause"));
          if (le && re
              && !ly_is_equal (le->get_property ("pitch"),
                               re->get_property ("pitch")))
            continue;
        }

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
      Stream_event *note = accidentals_[i].melodic_;
      Context *origin = accidentals_[i].origin_;

      int barnum = measure_number (origin);

      Pitch *pitch = unsmob<Pitch> (note->get_property ("pitch"));
      if (!pitch)
        continue;

      int n = pitch->get_notename ();
      int o = pitch->get_octave ();
      Rational a = pitch->get_alteration ();
      SCM key = scm_cons (scm_from_int (o), scm_from_int (n));

      Moment end_mp = measure_position (
          context (), unsmob<Duration> (note->get_property ("duration")));
      SCM position = scm_cons (scm_from_int (barnum), end_mp.smobbed_copy ());

      SCM localsig = SCM_EOL;
      while (origin
             && origin->where_defined (ly_symbol2scm ("localAlterations"),
                                       &localsig))
        {
          bool change = false;
          if (accidentals_[i].tied_
              && !(to_boolean (
                  accidentals_[i].accidental_->get_property ("forced"))))
            {
              /*
                Remember an alteration that is different both from
                that of the tied note and of the key signature.
              */
              localsig = ly_assoc_prepend_x (
                  localsig, key, scm_cons (ly_symbol2scm ("tied"), position));
              change = true;
            }
          else
            {
              /*
                not really correct if there is more than one
                note head with the same notename.
              */
              localsig = ly_assoc_prepend_x (
                  localsig, key, scm_cons (ly_rational2scm (a), position));
              change = true;
            }

          if (change)
            origin->set_property ("localAlterations", localsig);

          origin = origin->get_parent_context ();
        }
    }

  if (accidental_placement_)
    for (vsize i = 0; i < note_columns_.size (); i++)
      Separation_item::add_conditional_item (note_columns_[i],
                                             accidental_placement_);

  accidental_placement_ = 0;
  accidentals_.clear ();
  note_columns_.clear ();
  left_objects_.clear ();
  right_objects_.clear ();
}

void
Accidental_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  Stream_event *note = info.event_cause ();
  if (note
      && (note->in_event_class ("note-event")
          || note->in_event_class ("trill-span-event"))
      // option to skip accidentals on string harmonics
      && (to_boolean (get_property ("harmonicAccidentals"))
          || !scm_is_eq (info.grob ()->get_property ("style"),
                         ly_symbol2scm ("harmonic")))
      // ignore accidentals in non-printing voices like NullVoice
      && !to_boolean (info.context ()->get_property ("nullAccidentals")))
    {
      Accidental_entry entry;
      entry.head_ = info.grob ();
      entry.origin_engraver_
          = dynamic_cast<Engraver *> (info.origin_translator ());
      entry.origin_ = entry.origin_engraver_->context ();
      entry.melodic_ = note;

      accidentals_.push_back (entry);
    }
}

void
Accidental_engraver::acknowledge_end_tie (Grob_info info)
{
  ties_.push_back (dynamic_cast<Spanner *> (info.grob ()));
}

void
Accidental_engraver::acknowledge_note_column (Grob_info info)
{
  note_columns_.push_back (info.grob ());
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
  SCM sig = get_property ("keyAlterations");
  if (!scm_is_eq (last_keysig_, sig))
    update_local_key_signature (sig);
}

void
Accidental_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Accidental_engraver, arpeggio);
  ADD_ACKNOWLEDGER (Accidental_engraver, finger);
  ADD_ACKNOWLEDGER (Accidental_engraver, rhythmic_head);
  ADD_END_ACKNOWLEDGER (Accidental_engraver, tie);
  ADD_ACKNOWLEDGER (Accidental_engraver, note_column);
}

ADD_TRANSLATOR (Accidental_engraver,
                /* doc */
                "Make accidentals."
                "  Catch note heads, ties and notices key-change events."
                "  This engraver usually lives at Staff level, but"
                " reads the settings for Accidental at @code{Voice} level,"
                " so you can @code{\\override} them at @code{Voice}.",

                /* create */
                "Accidental "
                "AccidentalCautionary "
                "AccidentalPlacement "
                "AccidentalSuggestion ",

                /* read */
                "autoAccidentals "
                "autoCautionaries "
                "internalBarNumber "
                "extraNatural "
                "harmonicAccidentals "
                "accidentalGrouping "
                "keyAlterations "
                "localAlterations ",

                /* write */
                "localAlterations ");
