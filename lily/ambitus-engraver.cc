/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2020 Juergen Reuter <reuter@ipd.uka.de>

  Han-Wen Nienhuys <hanwen@xs4all.nl

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

#include "accidental-placement.hh"
#include "axis-group-interface.hh"
#include "item.hh"
#include "note-head.hh"
#include "pitch-interval.hh"
#include "pointer-group-interface.hh"
#include "protected-scm.hh"
#include "separation-item.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

class Ambitus_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Ambitus_engraver);

protected:
  void acknowledge_note_head (Grob_info);

  void process_music ();
  void stop_translation_timestep ();
  void finalize () override;
  void derived_mark () const override;

private:
  void create_ambitus ();
  Item *ambitus_;
  Item *group_;
  Drul_array<Item *> heads_;
  Drul_array<Item *> accidentals_;
  Drul_array<Stream_event *> causes_;
  Pitch_interval pitch_interval_;
  bool is_typeset_;
  int start_c0_;
  SCM start_key_sig_;
};

void
Ambitus_engraver::derived_mark () const
{
  scm_gc_mark (start_key_sig_);
}

void
Ambitus_engraver::create_ambitus ()
{
  ambitus_ = make_item ("AmbitusLine", SCM_EOL);
  group_ = make_item ("Ambitus", SCM_EOL);
  for (DOWN_and_UP (d))
    {
      heads_[d] = make_item ("AmbitusNoteHead", SCM_EOL);
      accidentals_[d] = make_item ("AmbitusAccidental", SCM_EOL);
      accidentals_[d]->set_parent (heads_[d], Y_AXIS);
      heads_[d]->set_object ("accidental-grob", accidentals_[d]->self_scm ());
      Axis_group_interface::add_element (group_, heads_[d]);
      Axis_group_interface::add_element (group_, accidentals_[d]);
    }

  ambitus_->set_parent (heads_[DOWN], X_AXIS);
  Axis_group_interface::add_element (group_, ambitus_);

  is_typeset_ = false;
}

Ambitus_engraver::Ambitus_engraver (Context *c) : Engraver (c)
{
  ambitus_ = 0;
  heads_.set (0, 0);
  accidentals_.set (0, 0);
  group_ = 0;
  is_typeset_ = false;
  start_key_sig_ = SCM_EOL;
}

void
Ambitus_engraver::process_music ()
{
  /*
   * Ensure that ambitus is created in the very first timestep
   */
  if (!ambitus_)
    create_ambitus ();
}

void
Ambitus_engraver::stop_translation_timestep ()
{
  if (ambitus_ && !is_typeset_)
    {
      SCM c_pos = get_property ("middleCPosition");
      SCM cue_pos = get_property ("middleCCuePosition");
      SCM ottavation = get_property ("ottavation");

      /*
       * \ottava reads middleCClefPosition and overrides
       * middleCOffset and middleCPosition ignoring previously
       * set values. Therefore
       *  1. \ottava is incompatible with non-default offset and
       *     position values (is this a bug? TODO)
       *  2. we don’t need to read these values and revert the
       *     changes \ottava made but we can just read the
       *     clef position.
       */
      if (Text_interface::is_markup (ottavation))
        start_c0_ = robust_scm2int (get_property ("middleCClefPosition"), 0);
      else if (scm_is_integer (c_pos) && !scm_is_integer (cue_pos))
        start_c0_ = scm_to_int (c_pos);
      else
        {
          int clef_pos
              = robust_scm2int (get_property ("middleCClefPosition"), 0);
          int offset = robust_scm2int (get_property ("middleCOffset"), 0);
          start_c0_ = clef_pos + offset;
        }

      start_key_sig_ = get_property ("keyAlterations");

      is_typeset_ = true;
    }
}

void
Ambitus_engraver::acknowledge_note_head (Grob_info info)
{
  Stream_event *nr = info.event_cause ();
  if (nr && nr->in_event_class ("note-event")
      && !to_boolean (info.grob ()->get_property ("ignore-ambitus")))
    {
      SCM p = nr->get_property ("pitch");
      /*
        If the engraver is added to a percussion context,
        filter out unpitched note heads.
      */
      if (!unsmob<Pitch> (p))
        return;
      Pitch pitch = *unsmob<Pitch> (p);
      Drul_array<bool> expands = pitch_interval_.add_point (pitch);
      if (expands[UP])
        causes_[UP] = nr;
      if (expands[DOWN])
        causes_[DOWN] = nr;
    }
}

void
Ambitus_engraver::finalize ()
{
  if (ambitus_ && !pitch_interval_.is_empty ())
    {
      Grob *accidental_placement
          = make_item ("AccidentalPlacement", accidentals_[DOWN]->self_scm ());

      SCM layout_proc = get_property ("staffLineLayoutFunction");

      for (DOWN_and_UP (d))
        {
          Pitch p = pitch_interval_[d];

          int pos;
          if (ly_is_procedure (layout_proc))
            pos = scm_to_int (scm_call_1 (layout_proc, p.smobbed_copy ()));
          else
            pos = p.steps ();

          heads_[d]->set_property ("cause", causes_[d]->self_scm ());
          heads_[d]->set_property ("staff-position",
                                   scm_from_int (start_c0_ + pos));

          SCM handle = scm_assoc (scm_cons (scm_from_int (p.get_octave ()),
                                            scm_from_int (p.get_notename ())),
                                  start_key_sig_);

          if (scm_is_false (handle))
            handle
                = scm_assoc (scm_from_int (p.get_notename ()), start_key_sig_);

          Rational sig_alter
              = (scm_is_true (handle))
                    ? robust_scm2rational (scm_cdr (handle), Rational (0))
                    : Rational (0);

          const Pitch other = pitch_interval_[-d];

          if (sig_alter == p.get_alteration ()
              && !((p.steps () == other.steps ())
                   && (p.get_alteration () != other.get_alteration ())))
            {
              accidentals_[d]->suicide ();
              heads_[d]->set_object ("accidental-grob", SCM_EOL);
            }
          else
            accidentals_[d]->set_property (
                "alteration", ly_rational2scm (p.get_alteration ()));
          Separation_item::add_conditional_item (heads_[d],
                                                 accidental_placement);
          Accidental_placement::add_accidental (accidental_placement,
                                                accidentals_[d], false, 0);
          Pointer_group_interface::add_grob (
              ambitus_, ly_symbol2scm ("note-heads"), heads_[d]);
        }

      Axis_group_interface::add_element (group_, accidental_placement);
    }
  else
    {
      for (DOWN_and_UP (d))
        {
          accidentals_[d]->suicide ();
          heads_[d]->suicide ();
        }

      ambitus_->suicide ();
    }
}

void
Ambitus_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Ambitus_engraver, note_head);
}

ADD_TRANSLATOR (Ambitus_engraver,
                /* doc */
                "Create an ambitus.",

                /* create */
                "AccidentalPlacement "
                "Ambitus "
                "AmbitusAccidental "
                "AmbitusLine "
                "AmbitusNoteHead ",

                /* read */
                "keyAlterations "
                "middleCPosition "
                "middleCClefPosition "
                "middleCCuePosition "
                "middleCOffset "
                "staffLineLayoutFunction ",

                /* write */
                "");
