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
#include "international.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Staff_symbol_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Staff_symbol_engraver);

protected:
  Drul_array<Stream_event *> span_events_;
  Spanner *span_;
  Spanner *finished_span_;
  bool first_start_;

protected:
  virtual void start_spanner ();
  virtual void stop_spanner ();

  void stop_translation_timestep ();
  virtual ~Staff_symbol_engraver ();
  void acknowledge_grob (Grob_info) override;
  void listen_staff_span (Stream_event *);
  void finalize () override;
  void process_music ();
  void derived_mark () const override;
};

void
Staff_symbol_engraver::derived_mark () const
{
  for (LEFT_and_RIGHT (d))
    {
      if (span_events_[d])
        scm_gc_mark (span_events_[d]->self_scm ());
    }
}

Staff_symbol_engraver::~Staff_symbol_engraver ()
{
  if (span_)
    {
      // Somehow finalize() was not called?
      programming_error ("Have a pending spanner in destructor.");
    }
}

Staff_symbol_engraver::Staff_symbol_engraver (Context *c) : Engraver (c)
{
  finished_span_ = 0;
  first_start_ = true;
  span_ = 0;
  span_events_.set (0, 0);
}

void
Staff_symbol_engraver::listen_staff_span (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  if (d)
    ASSIGN_EVENT_ONCE (span_events_[d], ev);
  else
    programming_error ("staff-span event has no direction");
}

void
Staff_symbol_engraver::process_music ()
{
  if (span_events_[STOP])
    {
      finished_span_ = span_;
      span_ = 0;
      if (first_start_)
        first_start_ = false;
    }

  if (span_events_[START] || (first_start_ && !span_events_[STOP]))
    start_spanner ();
}

void
Staff_symbol_engraver::start_spanner ()
{
  if (!span_)
    {
      span_ = make_spanner ("StaffSymbol", SCM_EOL);
      span_->set_bound (LEFT,
                        unsmob<Grob> (get_property ("currentCommandColumn")));
    }
}

void
Staff_symbol_engraver::stop_spanner ()
{
  if (!finished_span_)
    return;

  if (!finished_span_->get_bound (RIGHT))
    finished_span_->set_bound (
        RIGHT, unsmob<Grob> (get_property ("currentCommandColumn")));

  announce_end_grob (finished_span_, span_events_[STOP]
                                         ? span_events_[STOP]->self_scm ()
                                         : SCM_EOL);

  finished_span_ = 0;
}

void
Staff_symbol_engraver::stop_translation_timestep ()
{
  if ((span_events_[START] || first_start_) && span_)
    first_start_ = false;

  span_events_.set (0, 0);
  stop_spanner ();
}

void
Staff_symbol_engraver::finalize ()
{
  finished_span_ = span_;
  span_ = 0;
  stop_spanner ();
}

/*
  Todo: staff-symbol-referencer iface.
*/
void
Staff_symbol_engraver::acknowledge_grob (Grob_info s)
{
  if (span_ || finished_span_)
    {
      Spanner *my = span_ ? span_ : finished_span_;
      s.grob ()->set_object ("staff-symbol", my->self_scm ());
    }
}

void
Staff_symbol_engraver::boot ()
{
  ADD_LISTENER (Staff_symbol_engraver, staff_span);
  ADD_ACKNOWLEDGER (Staff_symbol_engraver, grob);
}

ADD_TRANSLATOR (Staff_symbol_engraver,
                /* doc */
                "Create the constellation of five (default) staff lines.",

                /* create */
                "StaffSymbol ",

                /* read */
                "",

                /* write */
                "");
