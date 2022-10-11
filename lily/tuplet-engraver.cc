/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "beam.hh"
#include "engraver.hh"
#include "international.hh"
#include "note-column.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "tuplet-bracket.hh"
#include "warn.hh"
#include "item.hh"
#include "moment.hh"

#include "translator.icc"

using std::vector;

struct Tuplet_description
{
  Stream_event *event_;
  Spanner *bracket_;
  Spanner *number_;

  bool full_length_;
  bool full_length_note_;
  Moment stop_moment_;
  Moment start_moment_;
  Moment length_;

  Tuplet_description ()
  {
    event_ = 0;
    full_length_note_ = false;
    full_length_ = false;
    bracket_ = 0;
    number_ = 0;
  }
};

class Tuplet_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Tuplet_engraver);

protected:
  vector<Tuplet_description> tuplets_;
  vector<Tuplet_description> new_tuplets_;
  vector<Tuplet_description> stopped_tuplets_;
  vector<Spanner *> last_tuplets_;

  void acknowledge_note_column (Grob_info_t<Item>);
  void acknowledge_script (Grob_info);
  void acknowledge_finger (Grob_info_t<Item>);
  void acknowledge_string_number (Grob_info_t<Item>);
  void add_script_to_all_tuplets (Item *);
  void listen_tuplet_span (Stream_event *);
  void finalize () override;
  void start_translation_timestep ();
  void process_music ();
};

void
Tuplet_engraver::listen_tuplet_span (Stream_event *ev)
{
  Direction dir = from_scm<Direction> (get_property (ev, "span-direction"));
  if (dir == START)
    {
      Tuplet_description d;
      d.event_ = ev;

      d.length_ = from_scm (get_property (d.event_, "length"), Moment (0));
      d.start_moment_ = now_mom ();
      d.stop_moment_ = now_mom () + d.length_;

      for (vsize i = 0; i < new_tuplets_.size (); i++)
        {
          /*
            discard duplicates.
          */
          if (new_tuplets_[i].stop_moment_ == d.stop_moment_)
            return;
        }

      new_tuplets_.push_back (d);
    }
  else if (dir == STOP)
    {
      if (tuplets_.size ())
        {
          stopped_tuplets_.push_back (tuplets_.back ());
          tuplets_.pop_back ();
        }
      else if (!from_scm<bool> (get_property (this, "skipTypesetting")))
        ev->debug_output (_ ("No tuplet to end"));
    }
  else
    ev->programming_error ("direction tuplet-span-event_ invalid.");
}

void
Tuplet_engraver::process_music ()
{
  /*
    This may happen if the end of a tuplet is part of a quoted voice.
   */
  auto now = now_mom ();
  for (vsize i = tuplets_.size (); i--;)
    {
      if (tuplets_[i].stop_moment_ == now)
        {
          stopped_tuplets_.push_back (tuplets_[i]);
          tuplets_.erase (tuplets_.begin () + i);
        }
    }

  for (auto &tuplet : stopped_tuplets_)
    {
      auto *bracket = tuplet.bracket_;
      auto *number = tuplet.number_;
      if (bracket)
        {
          if (tuplet.full_length_)
            {
              auto *col = unsmob<Item> (
                tuplet.full_length_note_
                  ? get_property (this, "currentMusicalColumn")
                  : get_property (this, "currentCommandColumn"));

              bracket->set_bound (RIGHT, col);
              number->set_bound (RIGHT, col);
            }
          else if (!bracket->get_bound (RIGHT))
            {
              if (auto *bound = bracket->get_bound (LEFT))
                {
                  bracket->set_bound (RIGHT, bound);
                  number->set_bound (RIGHT, bound);
                }
              else
                {
                  warning (_ ("omitting tuplet bracket with neither left nor "
                              "right bound"));
                  continue;
                }
            }
          // todo: scrap last_tuplets_, use stopped_tuplets_ only.
          // clear stopped_tuplets_ at start_translation_timestep
          last_tuplets_.push_back (bracket);
          last_tuplets_.push_back (number);
        }
    }
  stopped_tuplets_.clear ();

  tuplets_.insert (tuplets_.end (), new_tuplets_.begin (), new_tuplets_.end ());
  new_tuplets_.clear ();
  for (vsize j = tuplets_.size (); j > 0; j--)
    {
      /* i goes from size-1 downto 0, inclusively */
      vsize i = j - 1;

      if (tuplets_[i].bracket_)
        continue;

      tuplets_[i].full_length_
        = from_scm<bool> (get_property (this, "tupletFullLength"));
      tuplets_[i].full_length_note_
        = from_scm<bool> (get_property (this, "tupletFullLengthNote"));

      tuplets_[i].bracket_
        = make_spanner ("TupletBracket", tuplets_[i].event_->self_scm ());
      tuplets_[i].number_
        = make_spanner ("TupletNumber", tuplets_[i].event_->self_scm ());
      set_object (tuplets_[i].number_, "bracket",
                  tuplets_[i].bracket_->self_scm ());
      tuplets_[i].number_->set_x_parent (tuplets_[i].bracket_);
      tuplets_[i].number_->set_y_parent (tuplets_[i].bracket_);
      set_object (tuplets_[i].bracket_, "tuplet-number",
                  tuplets_[i].number_->self_scm ());
      tuplets_[i].stop_moment_.grace_part_ = 0;

      if (i + 1 < tuplets_.size () && tuplets_[i + 1].bracket_)
        Tuplet_bracket::add_tuplet_bracket (tuplets_[i].bracket_,
                                            tuplets_[i + 1].bracket_);

      if (i > 0 && tuplets_[i - 1].bracket_)
        Tuplet_bracket::add_tuplet_bracket (tuplets_[i - 1].bracket_,
                                            tuplets_[i].bracket_);
    }
}

void
Tuplet_engraver::acknowledge_note_column (Grob_info_t<Item> inf)
{
  for (vsize j = 0; j < tuplets_.size (); j++)
    if (tuplets_[j].bracket_)
      {
        auto *const i = inf.grob ();
        Tuplet_bracket::add_column (tuplets_[j].bracket_, i);
        add_bound_item (tuplets_[j].number_, i);
      }
}

void
Tuplet_engraver::acknowledge_script (Grob_info inf)
{
  // TODO: MultiMeasureRestScript is a Spanner.  Putting one inside a tuplet is
  // contrived, but is ignoring it really the most appropriate solution?  In
  // the future, will there be other Spanners with script-interface that we
  // can't so easily justify ignoring?  Should/could it be an Item instead?
  if (auto *item = dynamic_cast<Item *> (inf.grob ()))
    {
      // TODO: This is not the only place that dynamic-interface is excluded
      // from script-interface processing.  Is refactoring called for?
      if (!item->internal_has_interface (ly_symbol2scm ("dynamic-interface")))
        add_script_to_all_tuplets (item);
    }
}

void
Tuplet_engraver::acknowledge_finger (Grob_info_t<Item> inf)
{
  add_script_to_all_tuplets (inf.grob ());
}

void
Tuplet_engraver::acknowledge_string_number (Grob_info_t<Item> inf)
{
  add_script_to_all_tuplets (inf.grob ());
}

void
Tuplet_engraver::add_script_to_all_tuplets (Item *script)
{
  for (auto &tuplet : tuplets_)
    {
      if (tuplet.bracket_)
        Tuplet_bracket::add_script (tuplet.bracket_, script);
    }
}

void
Tuplet_engraver::start_translation_timestep ()
{
  last_tuplets_.clear ();
  /*
    May seem superfluous, but necessary for skipTypesetting.
   */
  new_tuplets_.clear ();
}

void
Tuplet_engraver::finalize ()
{
  if (from_scm<bool> (get_property (this, "tupletFullLength")))
    for (vsize i = 0; i < last_tuplets_.size (); i++)
      {
        Item *col = unsmob<Item> (get_property (this, "currentCommandColumn"));
        last_tuplets_[i]->set_bound (RIGHT, col);
      }
}

Tuplet_engraver::Tuplet_engraver (Context *c)
  : Engraver (c)
{
}

void
Tuplet_engraver::boot ()
{
  ADD_LISTENER (tuplet_span);
  ADD_ACKNOWLEDGER (note_column);
  ADD_ACKNOWLEDGER (script);
  ADD_ACKNOWLEDGER (finger);
  ADD_ACKNOWLEDGER (string_number);
}

ADD_TRANSLATOR (Tuplet_engraver,
                /* doc */
                R"(
Catch tuplet events and generate appropriate bracket.
                )",

                /* create */
                R"(
TupletBracket
TupletNumber
                )",

                /* read */
                R"(
tupletFullLength
tupletFullLengthNote
                )",

                /* write */
                R"(

                )");
