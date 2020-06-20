/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "directional-element-interface.hh"
#include "duration.hh"
#include "international.hh"
#include "item.hh"
#include "misc.hh"
#include "rhythmic-head.hh"
#include "script-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem-tremolo.hh"
#include "stem.hh"
#include "stream-event.hh"

#include "translator.icc"

using std::vector;

class Stem_engraver : public Engraver
{
  Grob *stem_;
  Grob *tremolo_;
  vector <Grob *> maybe_flags_;
  Stream_event *rhythmic_ev_;
  Stream_event *tremolo_ev_;
  bool tuplet_start_;

  TRANSLATOR_DECLARATIONS (Stem_engraver);

protected:
  void make_stem (Grob_info, bool);

  void listen_tremolo (Stream_event *);
  void listen_tuplet_span (Stream_event *);
  void acknowledge_rhythmic_head (Grob_info);
  void stop_translation_timestep ();
  void finalize () override;
  void kill_unused_flags ();
};

Stem_engraver::Stem_engraver (Context *c)
  : Engraver (c)
{
  tremolo_ev_ = 0;
  stem_ = 0;
  tremolo_ = 0;
  rhythmic_ev_ = 0;
  tuplet_start_ = false;
}

void
Stem_engraver::make_stem (Grob_info gi, bool tuplet_start)
{
  /* Announce the cause of the head as cause of the stem.  The
     stem needs a rhythmic structure to fit it into a beam.  */
  stem_ = make_item ("Stem", gi.grob ()->self_scm ());
  if (tuplet_start)
    set_property (stem_, "tuplet-start", SCM_BOOL_T);
  (void) make_item ("StemStub", gi.grob ()->self_scm ());
  if (tremolo_ev_)
    {
      /* Stem tremolo is never applied to a note by default,
         it must be requested.  But there is a default for the
         tremolo value:

         c4:8 c c:

         the first and last (quarter) note both get one tremolo flag.  */
      int requested_type
        = from_scm (get_property (tremolo_ev_, "tremolo-type"), 8);

      /*
        we take the duration log from the Event, since the duration-log
        for a note head is always <= 2.
      */
      Stream_event *ev = gi.event_cause ();
      Duration *dur = unsmob<Duration> (get_property (ev, "duration"));

      int tremolo_flags = intlog2 (requested_type) - 2
                          - (dur->duration_log () > 2 ? dur->duration_log () - 2 : 0);
      if (tremolo_flags <= 0)
        {
          tremolo_ev_->warning (_ ("tremolo duration is too long"));
          tremolo_flags = 0;
        }

      if (tremolo_flags)
        {
          tremolo_ = make_item ("StemTremolo", tremolo_ev_->self_scm ());

          /* The number of tremolo flags is the number of flags of the
             tremolo-type minus the number of flags of the note itself.  */
          set_property (tremolo_, "flag-count", to_scm (tremolo_flags));
          tremolo_->set_x_parent (stem_);
          set_object (stem_, "tremolo-flag", tremolo_->self_scm ());
          set_object (tremolo_, "stem", stem_->self_scm ());
        }
    }
}

void
Stem_engraver::acknowledge_rhythmic_head (Grob_info gi)
{
  if (Rhythmic_head::get_stem (gi.grob ()))
    return;

  Stream_event *cause = gi.event_cause ();
  if (!cause)
    return;
  Duration *d = unsmob<Duration> (get_property (cause, "duration"));
  if (!d)
    return;

  if (!stem_)
    make_stem (gi, tuplet_start_);

  int ds = Stem::duration_log (stem_);
  int dc = d->duration_log ();

  // half notes and quarter notes all have compatible stems.
  // Longas are done differently (oops?), so we can't unify
  // them with the other stemmed notes.
  if (ds == 1)
    ds = 2;
  if (dc == 1)
    dc = 2;
  // whole notes and brevis both have no stems
  if (ds == -1)
    ds = 0;
  if (dc == -1)
    dc = 0;

  if (ds != dc)
    {
      cause->warning (_f ("adding note head to incompatible stem (type = %d/%d)",
                          ds < 0 ? 1 << -ds : 1,
                          ds > 0 ? 1 << ds : 1));
      cause->warning (_ ("maybe input should specify polyphonic voices"));
    }

  Stem::add_head (stem_, gi.grob ());

  if (Stem::is_normal_stem (stem_)
      && Stem::duration_log (stem_) > 2
      && !(unsmob<Grob> (get_object (stem_, "flag"))))
    {
      Item *flag = make_item ("Flag", stem_->self_scm ());
      flag->set_x_parent (stem_);
      set_object (stem_, "flag", flag->self_scm ());
      maybe_flags_.push_back (flag);
    }
  if (tuplet_start_)
    set_property (stem_, "tuplet-start", SCM_BOOL_T);
}

void
Stem_engraver::kill_unused_flags ()
{
  for (vsize i = 0; i < maybe_flags_.size (); i++)
    if (unsmob<Grob> (get_object (maybe_flags_[i]->get_x_parent (), "beam")))
      maybe_flags_[i]->suicide ();
}

void
Stem_engraver::finalize ()
{
  kill_unused_flags ();
}

void
Stem_engraver::stop_translation_timestep ()
{
  if (scm_is_string (get_property (this, "whichBar")))
    kill_unused_flags ();

  tremolo_ = 0;
  if (stem_)
    {
      /* FIXME: junk these properties.  */
      SCM prop = get_property (this, "stemLeftBeamCount");
      if (scm_is_number (prop))
        {
          Stem::set_beaming (stem_, scm_to_int (prop), LEFT);
          context ()->unset_property (ly_symbol2scm ("stemLeftBeamCount"));
        }
      prop = get_property (this, "stemRightBeamCount");
      if (scm_is_number (prop))
        {
          Stem::set_beaming (stem_, scm_to_int (prop), RIGHT);
          context ()->unset_property (ly_symbol2scm ("stemRightBeamCount"));
        }
      stem_ = 0;
    }
  tuplet_start_ = false;
  tremolo_ev_ = 0;
}

void
Stem_engraver::listen_tuplet_span (Stream_event *ev)
{
  Direction dir = from_scm<Direction> (get_property (ev, "span-direction"));
  if (dir == START)
    {
      // set stem property if stem already exists
      if (stem_)
        set_property (stem_, "tuplet-start", SCM_BOOL_T);
      tuplet_start_ = true;  // stash the value for use in later creation
    }
}

void
Stem_engraver::listen_tremolo (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (tremolo_ev_, ev);
}

void
Stem_engraver::boot ()
{
  ADD_LISTENER (Stem_engraver, tuplet_span);
  ADD_LISTENER (Stem_engraver, tremolo);
  ADD_ACKNOWLEDGER (Stem_engraver, rhythmic_head);
}

ADD_TRANSLATOR (Stem_engraver,
                /* doc */
                "Create stems, flags and single-stem tremolos.  It also works"
                " together with the beam engraver for overriding beaming.",

                /* create */
                "Flag "
                "Stem "
                "StemStub "
                "StemTremolo ",

                /* read */
                "stemLeftBeamCount "
                "stemRightBeamCount "
                "whichBar ",

                /* write */
                ""
               );
