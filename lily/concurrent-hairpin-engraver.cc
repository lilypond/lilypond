/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2022 Mike Solomon <mike@mikesolomon.org>

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
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"
#include "item.hh"

#include "translator.icc"

using std::vector;

class Concurrent_hairpin_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Concurrent_hairpin_engraver);

protected:
  void acknowledge_hairpin (Grob_info);
  void acknowledge_end_hairpin (Grob_info);

  void stop_translation_timestep ();
  void finalize () override;

private:
  vector<Grob *> arriving_hairpins_;
  vector<Grob *> departing_hairpins_;
  vector<Grob *> hairpins_hanging_out_;
};

Concurrent_hairpin_engraver::Concurrent_hairpin_engraver (Context *c)
  : Engraver (c)
{
}

void
Concurrent_hairpin_engraver::acknowledge_hairpin (Grob_info info)
{
  arriving_hairpins_.push_back (info.grob ());
}

void
Concurrent_hairpin_engraver::acknowledge_end_hairpin (Grob_info info)
{
  departing_hairpins_.push_back (info.grob ());
}

void
Concurrent_hairpin_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < departing_hairpins_.size (); i++)
    for (vsize j = 0; j < hairpins_hanging_out_.size (); j++)
      if (departing_hairpins_[i] == hairpins_hanging_out_[j])
        {
          hairpins_hanging_out_.erase (hairpins_hanging_out_.begin () + j);
          break;
        }
  if (arriving_hairpins_.size ())
    {
      if (arriving_hairpins_.size () > 1)
        for (vsize i = 0; i < arriving_hairpins_.size () - 1; i++)
          for (vsize j = i + 1; j < arriving_hairpins_.size (); j++)
            {
              Pointer_group_interface::add_grob (
                arriving_hairpins_[i], ly_symbol2scm ("concurrent-hairpins"),
                arriving_hairpins_[j]);
              Pointer_group_interface::add_grob (
                arriving_hairpins_[j], ly_symbol2scm ("concurrent-hairpins"),
                arriving_hairpins_[i]);
            }

      for (vsize i = 0; i < arriving_hairpins_.size (); i++)
        for (vsize j = 0; j < hairpins_hanging_out_.size (); j++)
          {
            Pointer_group_interface::add_grob (
              arriving_hairpins_[i], ly_symbol2scm ("concurrent-hairpins"),
              hairpins_hanging_out_[j]);
            Pointer_group_interface::add_grob (
              hairpins_hanging_out_[j], ly_symbol2scm ("concurrent-hairpins"),
              arriving_hairpins_[i]);
          }
    }
  hairpins_hanging_out_.insert (hairpins_hanging_out_.end (),
                                arriving_hairpins_.begin (),
                                arriving_hairpins_.end ());
  arriving_hairpins_.resize (0);
  departing_hairpins_.resize (0);
}

void
Concurrent_hairpin_engraver::finalize ()
{
  hairpins_hanging_out_.resize (0);
}

void
Concurrent_hairpin_engraver::boot ()
{
  ADD_ACKNOWLEDGER (hairpin);
  ADD_END_ACKNOWLEDGER (hairpin);
}

ADD_TRANSLATOR (Concurrent_hairpin_engraver,
                /* doc */
                R"(
Collect concurrent hairpins.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
