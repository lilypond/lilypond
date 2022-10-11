/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "translator.icc"

using std::vector;

class Ledger_line_engraver : public Engraver
{
  Spanner *span_;
  vector<Grob *> ledgered_grobs_;

public:
  TRANSLATOR_DECLARATIONS (Ledger_line_engraver);

protected:
  void finalize () override;
  void process_music ();

  void acknowledge_ledgered (Grob_info);
  void acknowledge_staff_symbol (Grob_info_t<Spanner>);

  void start_spanner ();
  void stop_spanner ();
  void stop_translation_timestep ();
};

Ledger_line_engraver::Ledger_line_engraver (Context *c)
  : Engraver (c)
{
  span_ = 0;
}

void
Ledger_line_engraver::start_spanner ()
{
  assert (!span_);

  span_ = make_spanner ("LedgerLineSpanner", SCM_EOL);
  auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
  span_->set_bound (LEFT, col);
}

void
Ledger_line_engraver::stop_translation_timestep ()
{
  if (span_)
    {
      for (vsize i = 0; i < ledgered_grobs_.size (); i++)
        {
          if (!from_scm<bool> (get_property (ledgered_grobs_[i], "no-ledgers")))
            Pointer_group_interface::add_grob (
              span_, ly_symbol2scm ("note-heads"), ledgered_grobs_[i]);
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
      auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      span_->set_bound (RIGHT, col);
      span_ = 0;
    }
}

void
Ledger_line_engraver::acknowledge_staff_symbol (Grob_info_t<Spanner> s)
{
  if (!span_ || (span_->get_bound (LEFT) != s.grob ()->get_bound (LEFT)))
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

void
Ledger_line_engraver::boot ()
{
  ADD_ACKNOWLEDGER (ledgered);
  ADD_ACKNOWLEDGER (staff_symbol);
}

ADD_TRANSLATOR (Ledger_line_engraver,
                /* doc */
                R"(
Create the spanner to draw ledger lines, and notices objects that need ledger
lines.
                )",

                /* create */
                R"(
LedgerLineSpanner
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
