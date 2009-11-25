/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "engraver.hh"
#include "staff-symbol.hh"

class Ledger_line_engraver : public Engraver
{
  Spanner *span_;
  vector<Grob*> ledgered_grobs_;
  
public:
  TRANSLATOR_DECLARATIONS (Ledger_line_engraver);

protected:
  virtual void finalize ();
  void process_music ();

  DECLARE_ACKNOWLEDGER (ledgered);
  DECLARE_ACKNOWLEDGER (staff_symbol);

  void start_spanner ();
  void stop_spanner ();
  void stop_translation_timestep ();
};

Ledger_line_engraver::Ledger_line_engraver ()
{
  span_ = 0;
}

void
Ledger_line_engraver::start_spanner ()
{
  assert (!span_);

  span_ = make_spanner ("LedgerLineSpanner", SCM_EOL);
  span_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
}

void
Ledger_line_engraver::stop_translation_timestep ()
{
  if (span_)
    {
      for (vsize i = 0; i < ledgered_grobs_.size (); i++)
	{
	  if (!to_boolean (ledgered_grobs_[i]->get_property ("no-ledgers")))
	    Pointer_group_interface::add_grob (span_,
					       ly_symbol2scm ("note-heads"),
					       ledgered_grobs_[i]);
	}
    }

  ledgered_grobs_.clear ();
}

void
Ledger_line_engraver::process_music ()
{
  /*
    Need to do this, otherwise the first note might miss ledgers. 
  */
  if (!span_)
    start_spanner ();
}

void
Ledger_line_engraver::finalize ()
{
  stop_spanner ();
}

void
Ledger_line_engraver::stop_spanner ()
{
  if (span_)
    {
      span_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
      Pointer_group_interface::set_ordered (span_, ly_symbol2scm ("elements"), false);
      span_ = 0;
    }
}

void
Ledger_line_engraver::acknowledge_staff_symbol (Grob_info s)
{
  Spanner *sym = dynamic_cast<Spanner *> (s.grob ());

  if (!span_
      || (span_->get_bound (LEFT) != sym->get_bound (LEFT)))
    {
      stop_spanner ();
      start_spanner ();
    }
}

void
Ledger_line_engraver::acknowledge_ledgered (Grob_info s)
{
  ledgered_grobs_.push_back (s.grob ());
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Ledger_line_engraver, ledgered);
ADD_ACKNOWLEDGER (Ledger_line_engraver, staff_symbol);
ADD_TRANSLATOR (Ledger_line_engraver,
		/* doc */
		"Create the spanner to draw ledger lines, and notices"
		" objects that need ledger lines.",

		/* create */
		"LedgerLineSpanner ",

		/* read */
		"",

		/* write */
		""
		);
