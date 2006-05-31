/*
  staff-symbol-engraver.cc -- implement Staff_symbol_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "staff-symbol-engraver.hh"
#include "spanner.hh"

Staff_symbol_engraver::~Staff_symbol_engraver ()
{
  assert (!span_);
}

Staff_symbol_engraver::Staff_symbol_engraver ()
{
  finished_span_ = 0;
  first_start_ = true;
  span_ = 0;
  span_events_[LEFT] = 0;
  span_events_[RIGHT] = 0;
}

bool
Staff_symbol_engraver::try_music (Music *music)
{
  Direction d = to_dir (music->get_property ("span-direction"));
  if (d)
    {
      span_events_[d] = music;
      return true;
    }

  return false;
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

  if (span_events_[START]
      || (first_start_ && !span_events_[STOP]))
    start_spanner ();
}

void
Staff_symbol_engraver::start_spanner ()
{
  if (!span_)
    span_ = make_spanner ("StaffSymbol", SCM_EOL);
}

void
Staff_symbol_engraver::stop_spanner ()
{
  if (finished_span_ && !finished_span_->get_bound (RIGHT))
    finished_span_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
  finished_span_ = 0;
}

void
Staff_symbol_engraver::stop_translation_timestep ()
{
  if ((span_events_[START] || first_start_)
      && span_
      && !span_->get_bound (LEFT))
    {
      span_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
      first_start_ = false;
    }

  span_events_[START] = 0;
  span_events_[STOP] = 0;
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
  /*
    Perhaps should try to take SeparationItem as bound of the staff
    symbol?
  */
  if (span_ || finished_span_)
    {
      Spanner *my = span_ ? span_ : finished_span_;
      s.grob ()->set_object ("staff-symbol", my->self_scm ());
    }
}

#include "translator.icc"
ADD_ACKNOWLEDGER (Staff_symbol_engraver, grob);
ADD_TRANSLATOR (Staff_symbol_engraver,
		/* doc */ "Create the constellation of five (default) "
		"staff lines.",
		/* create */ "StaffSymbol",
		/* accept */ "staff-span-event",
		/* read */ "",
		/* write */ "");
