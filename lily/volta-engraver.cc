/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "international.hh"
#include "note-column.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "staff-symbol.hh"
#include "text-interface.hh"
#include "volta-bracket.hh"
#include "warn.hh"

#include "translator.icc"

/*
  Create Volta spanners, by reading repeatCommands  property, usually
  set by Volta_repeat_iterator.
*/
class Volta_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Volta_engraver);
protected:

  void acknowledge_bar_line (Grob_info);

  void stop_translation_timestep ();
  void process_music ();

  Moment started_mom_;
  Spanner *volta_bracket_ = nullptr;
  Spanner *end_volta_bracket_ = nullptr;
  Spanner *volta_spanner_ = nullptr;
};

Volta_engraver::Volta_engraver (Context *c)
  : Engraver (c)
{
}

void
Volta_engraver::process_music ()
{
  SCM cs = get_property (this, "repeatCommands");

  bool end = false;
  SCM bracket_text = SCM_EOL;
  while (scm_is_pair (cs))
    {
      SCM c = scm_car (cs);

      if (scm_is_pair (c)
          && scm_is_eq (scm_car (c), ly_symbol2scm ("volta"))
          && scm_is_pair (scm_cdr (c)))
        {
          SCM label = scm_cadr (c);
          if (scm_is_false (label))
            end = true;
          else
            bracket_text = label;
        }

      cs = scm_cdr (cs);
    }

  if (volta_bracket_ && !end)
    {
      SCM l (get_property (this, "voltaSpannerDuration"));
      Moment now = now_mom ();

      bool early_stop = unsmob<Moment> (l)
                        && *unsmob<Moment> (l) <= now - started_mom_;

      end = end || early_stop;
    }

  if (end && !volta_bracket_)
    /* fixme: be more verbose.  */
    warning (_ ("cannot end volta spanner"));
  else if (end)
    {
      end_volta_bracket_ = volta_bracket_;
      volta_bracket_ = 0;
    }

  if (volta_bracket_
      && (scm_is_string (bracket_text) || scm_is_pair (bracket_text)))
    {
      warning (_ ("already have a volta spanner, ending that one prematurely"));

      if (end_volta_bracket_)
        {
          warning (_ ("also already have an ended spanner"));
          warning (_ ("giving up"));
          return;
        }

      end_volta_bracket_ = volta_bracket_;
      volta_bracket_ = 0;
    }

  if (!volta_bracket_
      && Text_interface::is_markup (bracket_text))
    {
      started_mom_ = now_mom ();

      volta_bracket_ = make_spanner ("VoltaBracket", SCM_EOL);

      set_property (volta_bracket_, "text", bracket_text);

      if (!volta_spanner_)
        volta_spanner_ = make_spanner ("VoltaBracketSpanner", SCM_EOL);

      Axis_group_interface::add_element (volta_spanner_, volta_bracket_);
    }
}

void
Volta_engraver::acknowledge_bar_line (Grob_info i)
{
  Item *item = dynamic_cast<Item *> (i.grob ());
  if (volta_bracket_)
    Volta_bracket_interface::add_bar (volta_bracket_, item);
  if (end_volta_bracket_)
    Volta_bracket_interface::add_bar (end_volta_bracket_, item);

  if (volta_spanner_)
    Side_position_interface::add_support (volta_spanner_, i.grob ());
}

void
Volta_engraver::stop_translation_timestep ()
{
  Grob *cc = unsmob<Grob> (get_property (this, "currentCommandColumn"));
  Item *ci = dynamic_cast<Item *> (cc);

  if (end_volta_bracket_ && !end_volta_bracket_->get_bound (RIGHT))
    end_volta_bracket_->set_bound (RIGHT, ci);

  if (volta_spanner_ && end_volta_bracket_)
    volta_spanner_->set_bound (RIGHT, end_volta_bracket_->get_bound (RIGHT));

  if (end_volta_bracket_ && !volta_bracket_)
    {
      for (SCM s = get_property (this, "stavesFound"); scm_is_pair (s); s = scm_cdr (s))
        Side_position_interface::add_support (volta_spanner_, unsmob<Grob> (scm_car (s)));

      volta_spanner_ = 0;
    }

  if (end_volta_bracket_)
    {
      announce_end_grob (end_volta_bracket_, SCM_EOL);
      end_volta_bracket_ = 0;
    }

  if (volta_bracket_ && !volta_bracket_->get_bound (LEFT))
    volta_bracket_->set_bound (LEFT, ci);

  if (volta_spanner_ && volta_bracket_ && !volta_spanner_->get_bound (LEFT))
    volta_spanner_->set_bound (LEFT, volta_bracket_->get_bound (LEFT));
}

/*
  TODO: should attach volta to paper-column if no bar is found.
*/
void
Volta_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Volta_engraver, bar_line);
}

ADD_TRANSLATOR (Volta_engraver,
                /* doc */
                "Make volta brackets.",

                /* create */
                "VoltaBracket "
                "VoltaBracketSpanner ",

                /* read */
                "repeatCommands "
                "voltaSpannerDuration "
                "stavesFound ",

                /* write */
                ""
               );
