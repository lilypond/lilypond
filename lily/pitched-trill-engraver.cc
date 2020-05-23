/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver.hh"

#include "axis-group-interface.hh"
#include "context.hh"
#include "dots.hh"
#include "item.hh"
#include "note-head.hh"
#include "pitch.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

class Pitched_trill_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Pitched_trill_engraver);

protected:
  void acknowledge_note_head (Grob_info);
  void acknowledge_dots (Grob_info);
  void acknowledge_stem (Grob_info);
  void acknowledge_flag (Grob_info);
  void acknowledge_trill_spanner (Grob_info);
  void stop_translation_timestep ();

private:
  Item *trill_head_;
  Item *trill_group_;
  Item *trill_accidental_;

  vector<Grob *> heads_;

  void make_trill (Stream_event *);
};

Pitched_trill_engraver::Pitched_trill_engraver (Context *c)
  : Engraver (c)
{
  trill_head_ = 0;
  trill_group_ = 0;
  trill_accidental_ = 0;
}

void
Pitched_trill_engraver::acknowledge_dots (Grob_info info)
{
  heads_.push_back (info.grob ());
}
void
Pitched_trill_engraver::acknowledge_stem (Grob_info info)
{
  heads_.push_back (info.grob ());
}
void
Pitched_trill_engraver::acknowledge_flag (Grob_info info)
{
  heads_.push_back (info.grob ());
}
void
Pitched_trill_engraver::acknowledge_note_head (Grob_info info)
{
  heads_.push_back (info.grob ());
}

void
Pitched_trill_engraver::acknowledge_trill_spanner (Grob_info info)
{
  Stream_event *ev = info.event_cause ();
  if (ev
      && ev->in_event_class ("trill-span-event")
      && to_dir (get_property (ev, "span-direction")) == START
      && unsmob<Pitch> (get_property (ev, "pitch")))
    make_trill (ev);
}

void
Pitched_trill_engraver::make_trill (Stream_event *ev)
{
  SCM scm_pitch = get_property (ev, "pitch");
  Pitch *p = unsmob<Pitch> (scm_pitch);

  SCM keysig = get_property (this, "localAlterations");

  SCM key = scm_cons (scm_from_int (p->get_octave ()),
                      scm_from_int (p->get_notename ()));

  int bn = measure_number (context ());

  SCM handle = ly_assoc (key, keysig);
  if (scm_is_true (handle))
    {
      bool same_bar = (bn == robust_scm2int (scm_caddr (handle), 0));
      bool same_alt
        = (p->get_alteration () == robust_scm2rational (scm_cadr (handle), 0));

      if (!same_bar || (same_bar && !same_alt))
        handle = SCM_BOOL_F;
    }

  bool print_acc = scm_is_false (handle)
                   || p->get_alteration () == Rational (0)
                   || to_boolean (get_property (ev, "force-accidental"));

  if (trill_head_)
    {
      programming_error ("already have a trill head.");
      trill_head_ = 0;
    }

  trill_head_ = make_item ("TrillPitchHead", ev->self_scm ());
  SCM c0scm = get_property (this, "middleCPosition");

  int c0 = scm_is_number (c0scm) ? scm_to_int (c0scm) : 0;

  set_property (trill_head_, "staff-position",
                             scm_from_int (unsmob<Pitch> (scm_pitch)->steps ()
                                           + c0));

  trill_group_ = make_item ("TrillPitchGroup", ev->self_scm ());
  trill_group_->set_parent (trill_head_, Y_AXIS);

  Axis_group_interface::add_element (trill_group_, trill_head_);

  if (print_acc)
    {
      trill_accidental_ = make_item ("TrillPitchAccidental", ev->self_scm ());

      // fixme: naming -> alterations
      set_property (trill_accidental_, "alteration", ly_rational2scm (p->get_alteration ()));
      Side_position_interface::add_support (trill_accidental_, trill_head_);

      set_object (trill_head_, "accidental-grob", trill_accidental_->self_scm ());
      trill_accidental_->set_parent (trill_head_, Y_AXIS);
      Axis_group_interface::add_element (trill_group_, trill_accidental_);
    }
}

void
Pitched_trill_engraver::stop_translation_timestep ()
{
  if (trill_group_)
    for (vsize i = 0; i < heads_.size (); i++)
      Side_position_interface::add_support (trill_group_, heads_[i]);

  heads_.clear ();
  trill_head_ = 0;
  trill_group_ = 0;
  trill_accidental_ = 0;
}

void
Pitched_trill_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Pitched_trill_engraver, note_head);
  ADD_ACKNOWLEDGER (Pitched_trill_engraver, dots);
  ADD_ACKNOWLEDGER (Pitched_trill_engraver, stem);
  ADD_ACKNOWLEDGER (Pitched_trill_engraver, flag);
  ADD_ACKNOWLEDGER (Pitched_trill_engraver, trill_spanner);
}

ADD_TRANSLATOR (Pitched_trill_engraver,
                /* doc */
                "Print the bracketed note head after a note head with trill.",

                /* create */
                "TrillPitchHead "
                "TrillPitchAccidental "
                "TrillPitchGroup ",

                /* read */
                "",

                /* write */
                ""
               );
