/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2020 Han-Wen Nienhuys <hanwen@lilypond.org>


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
#include "item.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

/*
  TODO:


  * Detach from pedal specifics,

  * Also use this engraver for dynamics.
*/

struct Pedal_align_info
{
  Spanner *line_spanner_;
  Grob *carrying_item_;
  Spanner *carrying_spanner_;
  Spanner *finished_carrying_spanner_;

  Pedal_align_info () { clear (); }
  void clear ()
  {
    line_spanner_ = 0;
    carrying_spanner_ = 0;
    carrying_item_ = 0;
    finished_carrying_spanner_ = 0;
  }
  bool is_finished ()
  {
    bool do_continue = carrying_item_;

    do_continue |= (carrying_spanner_ && !finished_carrying_spanner_);
    do_continue |= (carrying_spanner_
                    && finished_carrying_spanner_ != carrying_spanner_);

    return !do_continue;
  }
};

class Piano_pedal_align_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Piano_pedal_align_engraver);

protected:
  void finalize () override;

  void acknowledge_piano_pedal_script (Grob_info);
  void acknowledge_piano_pedal_bracket (Grob_info);
  void acknowledge_note_column (Grob_info);

  void acknowledge_end_piano_pedal_bracket (Grob_info);

  void stop_translation_timestep ();
  void start_translation_timestep ();

private:
  enum Pedal_type
  {
    SOSTENUTO,
    SUSTAIN,
    UNA_CORDA,
    NUM_PEDAL_TYPES
  };
  Pedal_align_info pedal_info_[NUM_PEDAL_TYPES];
  vector<Grob *> supports_;

  Pedal_type get_grob_pedal_type (Grob_info g);
  Spanner *make_line_spanner (Pedal_type t, SCM);
};

Piano_pedal_align_engraver::Piano_pedal_align_engraver (Context *c)
    : Engraver (c)
{
}

void
Piano_pedal_align_engraver::start_translation_timestep ()
{
  supports_.clear ();
}

void
Piano_pedal_align_engraver::stop_translation_timestep ()
{
  for (int i = 0; i < NUM_PEDAL_TYPES; i++)
    {
      if (pedal_info_[i].line_spanner_)
        {

          if (pedal_info_[i].carrying_item_)
            {
              if (!pedal_info_[i].line_spanner_->get_bound (LEFT))
                pedal_info_[i].line_spanner_->set_bound (
                    LEFT, pedal_info_[i].carrying_item_);

              pedal_info_[i].line_spanner_->set_bound (
                  RIGHT, pedal_info_[i].carrying_item_);
            }
          else if (pedal_info_[i].carrying_spanner_
                   || pedal_info_[i].finished_carrying_spanner_)
            {
              if (!pedal_info_[i].line_spanner_->get_bound (LEFT)
                  && pedal_info_[i].carrying_spanner_->get_bound (LEFT))
                pedal_info_[i].line_spanner_->set_bound (
                    LEFT, pedal_info_[i].carrying_spanner_->get_bound (LEFT));

              if (pedal_info_[i].finished_carrying_spanner_)
                pedal_info_[i].line_spanner_->set_bound (
                    RIGHT,
                    pedal_info_[i].finished_carrying_spanner_->get_bound (
                        RIGHT));
            }

          for (vsize j = 0; j < supports_.size (); j++)
            {
              Side_position_interface::add_support (
                  pedal_info_[i].line_spanner_, supports_[j]);
            }

          if (pedal_info_[i].is_finished ())
            {
              announce_end_grob (pedal_info_[i].line_spanner_, SCM_EOL);
              pedal_info_[i].clear ();
            }
        }

      pedal_info_[i].carrying_item_ = 0;
    }
}

Piano_pedal_align_engraver::Pedal_type
Piano_pedal_align_engraver::get_grob_pedal_type (Grob_info g)
{
  if (g.event_cause ()->in_event_class ("sostenuto-event"))
    return SOSTENUTO;
  if (g.event_cause ()->in_event_class ("sustain-event"))
    return SUSTAIN;
  if (g.event_cause ()->in_event_class ("una-corda-event"))
    return UNA_CORDA;

  programming_error ("Unknown piano pedal type.  Defaulting to sustain");
  return SUSTAIN;
}

Spanner *
Piano_pedal_align_engraver::make_line_spanner (Pedal_type t, SCM cause)
{
  Spanner *sp = pedal_info_[t].line_spanner_;
  if (!sp)
    {
      switch (t)
        {
        case (SOSTENUTO):
          sp = make_spanner ("SostenutoPedalLineSpanner", cause);
          break;
        case (SUSTAIN):
          sp = make_spanner ("SustainPedalLineSpanner", cause);
          break;
        case (UNA_CORDA):
          sp = make_spanner ("UnaCordaPedalLineSpanner", cause);
          break;
        default:
          programming_error ("No pedal type fonud!");
          return sp;
        }

      pedal_info_[t].line_spanner_ = sp;
    }

  return sp;
}

void
Piano_pedal_align_engraver::acknowledge_note_column (Grob_info gi)
{
  supports_.push_back (gi.grob ());
}

void
Piano_pedal_align_engraver::acknowledge_piano_pedal_bracket (Grob_info gi)
{
  Pedal_type type = get_grob_pedal_type (gi);
  Grob *sp = make_line_spanner (type, gi.grob ()->self_scm ());

  Axis_group_interface::add_element (sp, gi.grob ());
  pedal_info_[type].carrying_spanner_ = dynamic_cast<Spanner *> (gi.grob ());
}

void
Piano_pedal_align_engraver::acknowledge_end_piano_pedal_bracket (Grob_info gi)
{
  Pedal_type type = get_grob_pedal_type (gi);
  pedal_info_[type].finished_carrying_spanner_
      = dynamic_cast<Spanner *> (gi.grob ());
}

void
Piano_pedal_align_engraver::acknowledge_piano_pedal_script (Grob_info gi)
{
  Pedal_type type = get_grob_pedal_type (gi);

  Grob *sp = make_line_spanner (type, gi.grob ()->self_scm ());
  Axis_group_interface::add_element (sp, gi.grob ());
  pedal_info_[type].carrying_item_ = gi.grob ();
}

void
Piano_pedal_align_engraver::finalize ()
{
  for (int i = 0; i < NUM_PEDAL_TYPES; i++)
    {
      if (pedal_info_[i].line_spanner_)
        {
          SCM cc = get_property ("currentCommandColumn");
          Item *c = unsmob<Item> (cc);
          pedal_info_[i].line_spanner_->set_bound (RIGHT, c);

          pedal_info_[i].clear ();
        }
    }
}

void
Piano_pedal_align_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Piano_pedal_align_engraver, note_column);
  ADD_ACKNOWLEDGER (Piano_pedal_align_engraver, piano_pedal_bracket);
  ADD_ACKNOWLEDGER (Piano_pedal_align_engraver, piano_pedal_script);
  ADD_END_ACKNOWLEDGER (Piano_pedal_align_engraver, piano_pedal_bracket);
}

ADD_TRANSLATOR (Piano_pedal_align_engraver,
                /* doc */
                "Align piano pedal symbols and brackets.",

                /* create */
                "SostenutoPedalLineSpanner "
                "SustainPedalLineSpanner "
                "UnaCordaPedalLineSpanner ",

                /* read */
                "currentCommandColumn ",

                /* write */
                "");
