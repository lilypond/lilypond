/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "span-event-listener.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Staff_symbol_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Staff_symbol_engraver);

protected:
  Unique_span_event_listener staff_span_listener_;
  Spanner *span_;
  Spanner *finished_span_;
  bool first_start_;

protected:
  virtual void start_spanner ();
  virtual void stop_spanner ();

  void stop_translation_timestep ();
  virtual ~Staff_symbol_engraver ();
  void acknowledge_grob (Grob_info) override;
  void finalize () override;
  void process_music ();
};

Staff_symbol_engraver::~Staff_symbol_engraver ()
{
  if (span_)
    {
      // Somehow finalize() was not called?
      programming_error ("Have a pending spanner in destructor.");
    }
}

Staff_symbol_engraver::Staff_symbol_engraver (Context *c)
  : Engraver (c)
{
  finished_span_ = 0;
  first_start_ = true;
  span_ = 0;
}

void
Staff_symbol_engraver::process_music ()
{
  if (staff_span_listener_.get_stop ())
    {
      finished_span_ = span_;
      span_ = 0;
      if (first_start_)
        first_start_ = false;
    }

  if (staff_span_listener_.get_start ()
      || (first_start_ && !staff_span_listener_.get_stop ()))
    start_spanner ();
}

void
Staff_symbol_engraver::start_spanner ()
{
  if (!span_)
    {
      span_ = make_spanner ("StaffSymbol", SCM_EOL);
      auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      span_->set_bound (LEFT, col);
      // A StaffSymbol's staff symbol is itself.
      set_object (span_, "staff-symbol", span_->self_scm ());
    }
}

void
Staff_symbol_engraver::stop_spanner ()
{
  if (!finished_span_)
    return;

  if (!finished_span_->get_bound (RIGHT))
    {
      auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      finished_span_->set_bound (RIGHT, col);
    }

  announce_end_grob (finished_span_,
                     staff_span_listener_.get_stop ()
                       ? staff_span_listener_.get_stop ()->self_scm ()
                       : SCM_EOL);

  finished_span_ = 0;
}

void
Staff_symbol_engraver::stop_translation_timestep ()
{
  if ((staff_span_listener_.get_start () || first_start_) && span_)
    first_start_ = false;

  staff_span_listener_.reset ();
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
      set_object (s.grob (), "staff-symbol", my->self_scm ());
    }
}

void
Staff_symbol_engraver::boot ()
{
  ADD_DELEGATE_LISTENER (staff_span);
  ADD_ACKNOWLEDGER (grob);
}

ADD_TRANSLATOR (Staff_symbol_engraver,
                /* doc */
                R"(
Create the constellation of five (default) staff lines.
                )",

                /* create */
                R"(
StaffSymbol
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
