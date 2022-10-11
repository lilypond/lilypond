/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys

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
#include "international.hh"
#include "item.hh"
#include "note-column.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "text-interface.hh"

class Ottava_spanner_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Ottava_spanner_engraver);

protected:
  void finalize () override;
  void start_translation_timestep ();

  void listen_ottava (Stream_event *);
  void acknowledge_note_column (Grob_info_t<Item>);

  void create_spanner ();
  void process_music ();
  void stop_translation_timestep ();
  void derived_mark () const override;

private:
  Stream_event *ottava_ev_ = nullptr;
  SCM ottavation_ = SCM_EOL;

  Spanner *span_ = nullptr;
  Spanner *finished_ = nullptr;

  void typeset_all ();
};

void
Ottava_spanner_engraver::derived_mark () const
{
  scm_gc_mark (ottavation_);
}

Ottava_spanner_engraver::Ottava_spanner_engraver (Context *c)
  : Engraver (c)
{
}

void
Ottava_spanner_engraver::start_translation_timestep ()
{
  set_property (context (), "ottavaStartNow", SCM_EOL);
}

void
Ottava_spanner_engraver::listen_ottava (Stream_event *ev)
{
  ottavation_ = get_property (ev, "ottava-number");
  SCM offset = to_scm (-7 * from_scm<int> (ottavation_));
  set_property (context (), "middleCOffset", offset);
  set_middle_C (context ());
  ottava_ev_ = ev;
}

void
Ottava_spanner_engraver::create_spanner ()
{
  span_ = make_spanner ("OttavaBracket", to_scm (ottava_ev_));

  // Respect user tweaks.
  if (scm_is_null (get_property_data (span_, "text")))
    {
      SCM ott = get_property (this, "ottavation");
      if (scm_is_null (ott))
        {
          SCM markups = get_property (this, "ottavationMarkups");
          ott = ly_assoc_get (ottavation_, markups, SCM_EOL);
          if (scm_is_null (ott))
            {
              warning (
                _f ("Could not find ottavation markup for %d octaves up.",
                    from_scm<int> (ottavation_)));
              ott = ly_string2scm ("");
            }
        }
      set_property (span_, "text", ott);
    }

  if (scm_is_null (get_property_data (span_, "direction")))
    {
      int offset = from_scm<int> (get_property (this, "middleCOffset"), 0);
      Direction d = (offset > 0) ? DOWN : UP;
      set_property (span_, "direction", to_scm (d));
    }
}

void
Ottava_spanner_engraver::process_music ()
{
  if (ottava_ev_)
    {
      finished_ = span_;
      span_ = nullptr;

      if (!from_scm<bool> (scm_zero_p (ottavation_)))
        {
          set_property (context (), "ottavaStartNow", SCM_BOOL_T);
          create_spanner ();
        }
    }
}

void
Ottava_spanner_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  if (span_)
    {
      auto *const it = info.grob ();
      Side_position_interface::add_support (span_, it);

      if (!span_->get_bound (LEFT))
        span_->set_bound (LEFT, it);
      span_->set_bound (RIGHT, it);
    }
}

void
Ottava_spanner_engraver::typeset_all ()
{
  if (finished_)
    {
      for (const auto d : {LEFT, RIGHT})
        {
          if (!finished_->get_bound (RIGHT))
            {
              Grob *e
                = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
              finished_->set_bound (d, e);
            }
        }

      finished_ = nullptr;
    }
}

void
Ottava_spanner_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      Grob *e = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
      span_->set_bound (LEFT, e);
    }

  typeset_all ();
  ottava_ev_ = nullptr;
}

void
Ottava_spanner_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    finished_ = span_;
  typeset_all ();
}

#include "translator.icc"

void
Ottava_spanner_engraver::boot ()
{
  ADD_LISTENER (ottava);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Ottava_spanner_engraver,
                /* doc */
                R"(
Create a text spanner when the ottavation property changes.
                )",

                /* create */
                R"(
OttavaBracket
                )",

                /* read */
                R"(
middleCOffset
ottavation
currentMusicalColumn
                )",

                /* write */
                R"(

                )");
