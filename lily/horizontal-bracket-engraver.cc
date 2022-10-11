/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"

#include "translator.icc"

using std::vector;

class Horizontal_bracket_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Horizontal_bracket_engraver);
  vector<Spanner *> bracket_stack_;
  vector<Spanner *> text_stack_;
  vector<Stream_event *> events_;
  vsize pop_count_;
  vsize push_count_;

  void stop_translation_timestep ();
  void process_music ();
  void acknowledge_note_column (Grob_info_t<Item>);
  void listen_note_grouping (Stream_event *);
};

Horizontal_bracket_engraver::Horizontal_bracket_engraver (Context *c)
  : Engraver (c)
{
  pop_count_ = 0;
  push_count_ = 0;
}

void
Horizontal_bracket_engraver::listen_note_grouping (Stream_event *ev)
{
  Direction d = from_scm<Direction> (get_property (ev, "span-direction"));

  if (d == STOP)
    {
      pop_count_++;
      if (pop_count_ > bracket_stack_.size ())
        ev->warning (_ ("do not have that many brackets"));
    }
  else
    {
      push_count_++;
      events_.push_back (ev);
    }

  if (pop_count_ && push_count_)
    ev->warning (_ ("conflicting note group events"));
}

void
Horizontal_bracket_engraver::acknowledge_note_column (Grob_info_t<Item> gi)
{
  for (vsize i = 0; i < bracket_stack_.size (); i++)
    {
      Side_position_interface::add_support (bracket_stack_[i], gi.grob ());
      Pointer_group_interface::add_grob (bracket_stack_[i],
                                         ly_symbol2scm ("columns"), gi.grob ());
      add_bound_item (bracket_stack_[i], gi.grob ());
      add_bound_item (text_stack_[i], gi.grob ());
    }
}

void
Horizontal_bracket_engraver::process_music ()
{
  for (vsize k = 0; k < push_count_; k++)
    {
      Spanner *sp = make_spanner ("HorizontalBracket", events_[k]->self_scm ());

      Spanner *hbt = make_spanner ("HorizontalBracketText", sp->self_scm ());

      set_object (sp, "bracket-text", hbt->self_scm ());

      Side_position_interface::add_support (hbt, sp);

      hbt->set_x_parent (sp);
      hbt->set_y_parent (sp);
      set_object (hbt, "bracket", sp->self_scm ());

      for (vsize i = 0; i < bracket_stack_.size (); i++)
        /* sp is the smallest, it should be added to the bigger brackets.  */
        {
          Side_position_interface::add_support (bracket_stack_[i], sp);
          Side_position_interface::add_support (bracket_stack_[i], hbt);
        }

      bracket_stack_.push_back (sp);
      text_stack_.push_back (hbt);
    }
}

void
Horizontal_bracket_engraver::stop_translation_timestep ()
{
  for (vsize i = pop_count_; i--;)
    {
      if (bracket_stack_.size ())
        bracket_stack_.pop_back ();
      if (text_stack_.size ())
        text_stack_.pop_back ();
    }
  pop_count_ = 0;
  push_count_ = 0;
  events_.clear ();
}

void
Horizontal_bracket_engraver::boot ()
{
  ADD_LISTENER (note_grouping);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Horizontal_bracket_engraver,
                /* doc */
                R"(
Create horizontal brackets over notes for musical analysis purposes.
                )",

                /* create */
                R"(
HorizontalBracket
HorizontalBracketText
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
