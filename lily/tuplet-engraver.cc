/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "tuplet-description.hh"
#include "warn.hh"
#include "item.hh"
#include "moment.hh"

#include "translator.icc"

class Tuplet_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Tuplet_engraver);

protected:
  std::vector<Tuplet_description *> tuplets_;
  std::vector<Tuplet_description *> new_tuplets_;
  std::vector<Tuplet_description *> stopped_tuplets_;

  void derived_mark () const override;

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
Tuplet_engraver::derived_mark () const
{
  for (auto const &vec : {tuplets_, new_tuplets_, stopped_tuplets_})
    for (Tuplet_description *tuplet : vec)
      scm_gc_mark (tuplet->self_scm ());
}

void
Tuplet_engraver::listen_tuplet_span (Stream_event *ev)
{
  Direction dir = from_scm<Direction> (get_property (ev, "span-direction"));
  if (dir == START)
    {
      Tuplet_description *const new_tuplet
        = new Tuplet_description (ev, now_mom ());
      //
      for (Tuplet_description const *existing : new_tuplets_)
        if (*existing == *new_tuplet)
          // do not add already-existing tuplet
          return;

      new_tuplets_.push_back (new_tuplet);
      new_tuplet->unprotect ();
    }
  else if (dir == STOP)
    {
      if (!tuplets_.empty ())
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
  Moment now = now_mom ();
  while (!tuplets_.empty () && tuplets_.back ()->stop_moment_ == now)
    {
      stopped_tuplets_.push_back (tuplets_.back ());
      tuplets_.pop_back ();
    }

  for (Tuplet_description const *tuplet : stopped_tuplets_)
    {
      if (tuplet->bracket_)
        {
          if (Grob *left = tuplet->bracket_->get_bound (LEFT))
            {
              if (tuplet->full_length_)
                {
                  auto *col = unsmob<Item> (
                    tuplet->full_length_note_
                      ? get_property (this, "currentMusicalColumn")
                      : get_property (this, "currentCommandColumn"));

                  tuplet->bracket_->set_bound (RIGHT, col);
                  tuplet->number_->set_bound (RIGHT, col);
                }
              else if (!tuplet->bracket_->get_bound (RIGHT))
                {
                  // This tuplet only spans one note, e.g.,
                  // \tuplet 3/2 { s8 c'8 s8 }.
                  tuplet->bracket_->set_bound (RIGHT, left);
                  tuplet->number_->set_bound (RIGHT, left);
                }
            }
          else
            {
              // This tuplet spans no notes at all, e.g.,
              // \tuplet 3/2 { s8 s8 s8 }.  Remove it.
              tuplet->bracket_->suicide ();
              tuplet->number_->suicide ();
            }
        }
    }

  tuplets_.reserve (tuplets_.size () + new_tuplets_.size ());
  for (Tuplet_description *tuplet : new_tuplets_)
    {
      if (!tuplets_.empty ())
        tuplet->parent_ = tuplets_.back ();
      tuplets_.push_back (tuplet);
    }
  new_tuplets_.clear ();

  set_property (context (), "currentTupletDescription",
                tuplets_.empty () ? SCM_EOL : tuplets_.back ()->self_scm ());

  for (vsize i = tuplets_.size (); i--;)
    {
      if (tuplets_[i]->bracket_)
        continue;

      tuplets_[i]->full_length_
        = from_scm<bool> (get_property (this, "tupletFullLength"));
      tuplets_[i]->full_length_note_
        = from_scm<bool> (get_property (this, "tupletFullLengthNote"));

      tuplets_[i]->bracket_
        = make_spanner ("TupletBracket", tuplets_[i]->event_->self_scm ());
      tuplets_[i]->number_
        = make_spanner ("TupletNumber", tuplets_[i]->event_->self_scm ());

      Spanner *const bracket = tuplets_[i]->bracket_;
      Spanner *const number = tuplets_[i]->number_;
      set_object (number, "bracket", bracket->self_scm ());
      set_object (bracket, "tuplet-number", number->self_scm ());
      number->set_x_parent (bracket);
      number->set_y_parent (bracket);
      tuplets_[i]->stop_moment_.grace_part_ = 0;

      if (i + 1 < tuplets_.size () && tuplets_[i + 1]->bracket_)
        Tuplet_bracket::add_tuplet_bracket (bracket, tuplets_[i + 1]->bracket_);

      if (i > 0 && tuplets_[i - 1]->bracket_)
        Tuplet_bracket::add_tuplet_bracket (tuplets_[i - 1]->bracket_, bracket);
    }
}

void
Tuplet_engraver::acknowledge_note_column (Grob_info_t<Item> inf)
{
  for (Tuplet_description const *tuplet : tuplets_)
    if (tuplet->bracket_)
      {
        auto *const i = inf.grob ();
        Tuplet_bracket::add_column (tuplet->bracket_, i);
        add_bound_item (tuplet->number_, i);
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
  for (Tuplet_description *tuplet : tuplets_)
    if (tuplet->bracket_)
      Tuplet_bracket::add_script (tuplet->bracket_, script);
}

void
Tuplet_engraver::start_translation_timestep ()
{
  stopped_tuplets_.clear ();
  /*
    May seem superfluous, but necessary for skipTypesetting.
   */
  new_tuplets_.clear ();
}

void
Tuplet_engraver::finalize ()
{
  // If tupletFullLengthNote is used, fix up bounds to avoid grobs extending to
  // the musical column of the last time step, which is after the end of the
  // piece.
  Item *col = unsmob<Item> (get_property (this, "currentCommandColumn"));
  for (Tuplet_description *desc : stopped_tuplets_)
    if (desc->full_length_note_)
      for (Spanner *g : {desc->bracket_, desc->number_})
        g->set_bound (RIGHT, col);
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
