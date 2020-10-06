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

#include "engraver-group.hh"

#include "item.hh"
#include "international.hh"
#include "misc.hh"
#include "moment.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/**
   generate time_signatures.
*/
class Time_signature_engraver : public Engraver
{
  Item *time_signature_;
  SCM last_time_fraction_;
  SCM time_cause_;

protected:
  void derived_mark () const override;
  void stop_translation_timestep ();
  void process_music ();
public:
  TRANSLATOR_DECLARATIONS (Time_signature_engraver);
  void listen_time_signature (Stream_event *);
};

void
Time_signature_engraver::derived_mark () const
{
  scm_gc_mark (last_time_fraction_);
  scm_gc_mark (time_cause_);
}

Time_signature_engraver::Time_signature_engraver (Context *c)
  : Engraver (c)
{
  time_signature_ = 0;
  time_cause_ = SCM_EOL;
  last_time_fraction_ = SCM_BOOL_F;
}

void
Time_signature_engraver::listen_time_signature (Stream_event *ev)
{
  time_cause_ = ev->self_scm ();
}

void
Time_signature_engraver::process_music ()
{
  if (time_signature_)
    return;

  SCM fr = get_property (this, "timeSignatureFraction");
  if (!scm_is_eq (last_time_fraction_, fr) && scm_is_pair (fr))
    {
      time_signature_ = make_item ("TimeSignature", time_cause_);
      set_property (time_signature_, "fraction", fr);

      if (scm_is_false (last_time_fraction_))
        set_property (time_signature_, "break-visibility",
                      get_property (this, "initialTimeSignatureVisibility"));

      int den = scm_to_int (scm_cdr (fr));
      if (den != (1 << intlog2 (den)))
        {
          /*
            Todo: should make typecheck?

            OTOH, Tristan Keuris writes 8/20 in his Intermezzi.
          */
          time_signature_->warning (_f ("strange time signature found: %d/%d",
                                        from_scm<int> (scm_car (fr)),
                                        den));
        }

      last_time_fraction_ = fr;
    }
}

void
Time_signature_engraver::stop_translation_timestep ()
{
  if (time_signature_ && !scm_is_null (time_cause_))
    {
      Moment *mp = unsmob<Moment> (get_property (this, "measurePosition"));
      if (mp && (mp->main_part_ > Rational (0))
          && !from_scm<bool> (get_property (this, "partialBusy")))
        time_signature_->warning (_ ("mid-measure time signature without \\partial"));
    }

  time_signature_ = 0;
  time_cause_ = SCM_EOL;
}

void
Time_signature_engraver::boot ()
{
  ADD_LISTENER (Time_signature_engraver, time_signature);
}

ADD_TRANSLATOR (Time_signature_engraver,
                /* doc */
                "Create a @ref{TimeSignature} whenever"
                " @code{timeSignatureFraction} changes.",

                /* create */
                "TimeSignature ",

                /* read */
                "initialTimeSignatureVisibility "
                "partialBusy "
                "timeSignatureFraction ",

                /* write */
                ""
               );
