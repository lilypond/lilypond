/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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
#include "hairpin.hh"
#include "international.hh"
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "self-alignment-interface.hh"
#include "span-event-listener.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

using std::string;

class Dynamic_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Dynamic_engraver);
  void acknowledge_note_column (Grob_info_t<Item>);
  void listen_absolute_dynamic (Stream_event *);
  void listen_break_dynamic_span (Stream_event *);

protected:
  void process_music ();
  void stop_translation_timestep ();
  void finalize () override;

private:
  SCM get_property_setting (Stream_event const *evt, char const *evprop,
                            char const *ctxprop);
  string get_spanner_type (Stream_event const *ev);

private:
  Unique_span_event_listener span_dynamic_listener_;
  Spanner *current_spanner_ = nullptr;
  Spanner *finished_spanner_ = nullptr;

  Item *script_ = nullptr;
  Stream_event *script_event_ = nullptr;
  bool end_new_spanner_ = false;
};

Dynamic_engraver::Dynamic_engraver (Context *c)
  : Engraver (c)
{
}

void
Dynamic_engraver::listen_absolute_dynamic (Stream_event *ev)
{
  assign_event_once (script_event_, ev);
}

void
Dynamic_engraver::listen_break_dynamic_span (Stream_event *)
{
  // Case 1: Already have a start dynamic event -> break applies to new
  //         spanner (created later) -> set a flag
  // Case 2: no new spanner, but spanner already active -> break it now
  if (span_dynamic_listener_.get_start ())
    end_new_spanner_ = true;
  else if (current_spanner_)
    set_property (current_spanner_, "spanner-broken", SCM_BOOL_T);
}

SCM
Dynamic_engraver::get_property_setting (Stream_event const *evt,
                                        char const *evprop, char const *ctxprop)
{
  SCM spanner_type = get_property (evt, evprop);
  if (scm_is_null (spanner_type))
    spanner_type = get_property (this, ctxprop);
  return spanner_type;
}

void
Dynamic_engraver::process_music ()
{
  if (current_spanner_)
    {
      auto *ender = span_dynamic_listener_.get_stop ();
      if (!ender)
        ender = script_event_;
      if (!ender)
        ender = span_dynamic_listener_.get_start ();

      if (ender)
        {
          finished_spanner_ = current_spanner_;
          announce_end_grob (finished_spanner_, ender->self_scm ());
          current_spanner_ = nullptr;
        }
    }

  if (auto *const starter = span_dynamic_listener_.get_start ())
    {
      string start_type = get_spanner_type (starter);
      SCM cresc_type = get_property_setting (starter, "span-type",
                                             (start_type + "Spanner").c_str ());

      if (scm_is_eq (cresc_type, ly_symbol2scm ("text")))
        {
          current_spanner_
            = make_spanner ("DynamicTextSpanner", starter->self_scm ());

          SCM text = get_property_setting (starter, "span-text",
                                           (start_type + "Text").c_str ());
          if (Text_interface::is_markup (text))
            set_property (current_spanner_, "text", text);
          /*
            If the line of a text spanner is hidden, end the alignment spanner
            early: this allows dynamics to be spaced individually instead of
            being linked together.
          */
          if (scm_is_eq (get_property (current_spanner_, "style"),
                         ly_symbol2scm ("none")))
            set_property (current_spanner_, "spanner-broken", SCM_BOOL_T);
        }
      else
        {
          if (!scm_is_eq (cresc_type, ly_symbol2scm ("hairpin")))
            {
              string as_string = ly_scm_write_string (cresc_type);
              starter->warning (
                _f ("unknown crescendo style: %s\ndefaulting to hairpin.",
                    as_string.c_str ()));
            }
          current_spanner_ = make_spanner ("Hairpin", starter->self_scm ());
        }
      // if we have a break-dynamic-span event right after the start dynamic, break the new spanner immediately
      if (end_new_spanner_)
        {
          set_property (current_spanner_, "spanner-broken", SCM_BOOL_T);
          end_new_spanner_ = false;
        }
      if (finished_spanner_)
        {
          if (has_interface<Hairpin> (finished_spanner_))
            Pointer_group_interface::add_grob (
              finished_spanner_, ly_symbol2scm ("adjacent-spanners"),
              current_spanner_);
          if (has_interface<Hairpin> (current_spanner_))
            Pointer_group_interface::add_grob (
              current_spanner_, ly_symbol2scm ("adjacent-spanners"),
              finished_spanner_);
        }
    }

  if (script_event_)
    {
      script_ = make_item ("DynamicText", script_event_->self_scm ());
      set_property (script_, "text", get_property (script_event_, "text"));

      if (finished_spanner_)
        finished_spanner_->set_bound (RIGHT, script_);
      if (current_spanner_)
        current_spanner_->set_bound (LEFT, script_);
    }
}

void
Dynamic_engraver::stop_translation_timestep ()
{
  if (finished_spanner_ && !finished_spanner_->get_bound (RIGHT))
    finished_spanner_->set_bound (
      RIGHT, unsmob<Grob> (get_property (this, "currentMusicalColumn")));

  if (current_spanner_ && !current_spanner_->get_bound (LEFT))
    current_spanner_->set_bound (
      LEFT, unsmob<Grob> (get_property (this, "currentMusicalColumn")));
  script_ = nullptr;
  script_event_ = nullptr;
  span_dynamic_listener_.reset ();
  finished_spanner_ = nullptr;
  end_new_spanner_ = false;
}

void
Dynamic_engraver::finalize ()
{
  if (current_spanner_ && !current_spanner_->is_live ())
    current_spanner_ = nullptr;

  if (current_spanner_)
    {
      auto *const event = current_spanner_->event_cause ();
      current_spanner_->warning (
        _f ("unterminated %s", get_spanner_type (event).c_str ()));
      current_spanner_->suicide ();
      current_spanner_ = nullptr;
    }
}

string
Dynamic_engraver::get_spanner_type (Stream_event const *ev)
{
  string type;
  SCM start_sym = scm_car (get_property (ev, "class"));

  if (scm_is_eq (start_sym, ly_symbol2scm ("decrescendo-event")))
    type = "decrescendo";
  else if (scm_is_eq (start_sym, ly_symbol2scm ("crescendo-event")))
    type = "crescendo";
  else
    programming_error ("unknown dynamic spanner type");

  return type;
}

void
Dynamic_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  if (script_ && !script_->get_x_parent ())
    {
      extract_grob_set (info.grob (), "note-heads", heads);
      /*
        Spacing constraints may require dynamics to be attached to rests,
        so check for a rest if this note column has no note heads.
      */
      Grob *x_parent
        = (heads.size () ? info.grob ()
                         : unsmob<Grob> (get_object (info.grob (), "rest")));
      if (x_parent)
        script_->set_x_parent (x_parent);
    }

  if (current_spanner_ && !current_spanner_->get_bound (LEFT))
    current_spanner_->set_bound (LEFT, info.grob ());
  if (finished_spanner_ && !finished_spanner_->get_bound (RIGHT))
    finished_spanner_->set_bound (RIGHT, info.grob ());
}

void
Dynamic_engraver::boot ()
{
  ADD_LISTENER (absolute_dynamic);
  ADD_LISTENER (break_dynamic_span);
  ADD_DELEGATE_LISTENER (span_dynamic);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Dynamic_engraver,
                /* doc */
                R"(
Create hairpins, dynamic texts and dynamic text spanners.
                )",

                /* create */
                R"(
DynamicTextSpanner
DynamicText
Hairpin
                )",

                /* read */
                R"(
crescendoSpanner
crescendoText
currentMusicalColumn
decrescendoSpanner
decrescendoText
                )",

                /* write */
                R"(

                )");
