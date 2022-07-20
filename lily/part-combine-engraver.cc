/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>

  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "note-head.hh"
#include "side-position-interface.hh"
#include "simple-event-listener.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "item.hh"

#include "translator.icc"

class Part_combine_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Part_combine_engraver);

protected:
  void acknowledge_note_head (Grob_info);
  void acknowledge_stem (Grob_info);

  void listen_part_combine (Stream_event *);
  void process_music ();
  void stop_translation_timestep ();
  void create_item (Stream_event *ev);

private:
  Item *text_;
  Stream_event *new_event_; // Event happened at this moment
  Boolean_event_listener note_listener_;
  // Event possibly from an earlier moment waiting to create a text:
  Stream_event *waiting_event_;
};

void
Part_combine_engraver::listen_part_combine (Stream_event *ev)
{
  assign_event_once (new_event_, ev);
  // If two events occur at the same moment, discard the second as the
  // warning indicates:
  waiting_event_ = new_event_;
}

Part_combine_engraver::Part_combine_engraver (Context *c)
  : Engraver (c)
{
  text_ = 0;
  new_event_ = 0;
  waiting_event_ = 0;
}

void
Part_combine_engraver::create_item (Stream_event *ev)
{
  SCM what = scm_car (get_property (ev, "class"));
  SCM text = SCM_EOL;
  if (scm_is_eq (what, ly_symbol2scm ("solo-one-event")))
    text = get_property (this, "soloText");
  else if (scm_is_eq (what, ly_symbol2scm ("solo-two-event")))
    text = get_property (this, "soloIIText");
  else if (scm_is_eq (what, ly_symbol2scm ("unisono-event")))
    text = get_property (this, "aDueText");

  if (Text_interface::is_markup (text))
    {
      text_ = make_item ("CombineTextScript", ev->self_scm ());
      set_property (text_, "text", text);
    }
}

void
Part_combine_engraver::process_music ()
{
  if (waiting_event_
      && from_scm<bool> (get_property (this, "printPartCombineTexts")))
    {
      if (note_listener_.heard ()
          || !from_scm<bool> (get_property (this, "partCombineTextsOnNote")))
        {
          create_item (waiting_event_);
          waiting_event_ = 0;
        }
    }
}

void
Part_combine_engraver::acknowledge_note_head (Grob_info i)
{
  if (text_)
    {
      Grob *t = text_;
      Side_position_interface::add_support (t, i.grob ());
      if (Side_position_interface::is_on_x_axis (t) && !t->get_y_parent ())
        t->set_y_parent (i.grob ());
    }
}

void
Part_combine_engraver::acknowledge_stem (Grob_info i)
{
  if (text_)
    Side_position_interface::add_support (text_, i.grob ());
}

void
Part_combine_engraver::stop_translation_timestep ()
{
  text_ = 0;
  new_event_ = 0;
  note_listener_.reset ();
}

void
Part_combine_engraver::boot ()
{
  ADD_LISTENER (part_combine);
  ADD_DELEGATE_LISTENER (note);
  ADD_ACKNOWLEDGER (note_head);
  ADD_ACKNOWLEDGER (stem);
}

ADD_TRANSLATOR (Part_combine_engraver,
                /* doc */
                R"(
Part combine engraver for orchestral scores: Print markings @q{a2}, @q{Solo},
@q{Solo II}, and @q{unisono}.
                )",

                /* create */
                R"(
CombineTextScript
                )",

                /* read */
                R"(
printPartCombineTexts
partCombineTextsOnNote
soloText
soloIIText
aDueText
                )",

                /* write */
                R"(

                )");
