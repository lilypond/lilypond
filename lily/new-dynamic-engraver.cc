/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2009 Han-Wen Nienhuys <hanwen@lilypond.org>

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
#include "spanner.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

class New_dynamic_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (New_dynamic_engraver);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_TRANSLATOR_LISTENER (absolute_dynamic);
  DECLARE_TRANSLATOR_LISTENER (span_dynamic);

protected:
  virtual void process_music ();
  virtual void stop_translation_timestep ();
  virtual void finalize ();

private:
  SCM get_property_setting (Stream_event *evt, char const *evprop,
			    char const *ctxprop);
  string get_spanner_type (Stream_event *ev);

  Drul_array<Stream_event *> accepted_spanevents_drul_;
  Spanner *current_spanner_;
  Spanner *finished_spanner_;

  Item *script_;
  Stream_event *script_event_;
  Stream_event *current_span_event_;
};

New_dynamic_engraver::New_dynamic_engraver ()
{
  script_event_ = 0;
  current_span_event_ = 0;
  script_ = 0;
  finished_spanner_ = 0;
  current_spanner_ = 0;
  accepted_spanevents_drul_.set (0, 0);
}

IMPLEMENT_TRANSLATOR_LISTENER (New_dynamic_engraver, absolute_dynamic);
void
New_dynamic_engraver::listen_absolute_dynamic (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (script_event_, ev);
}

IMPLEMENT_TRANSLATOR_LISTENER (New_dynamic_engraver, span_dynamic);
void
New_dynamic_engraver::listen_span_dynamic (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));

  ASSIGN_EVENT_ONCE (accepted_spanevents_drul_[d], ev);
}

SCM
New_dynamic_engraver::get_property_setting (Stream_event *evt,
					    char const *evprop,
					    char const *ctxprop)
{
  SCM spanner_type = evt->get_property (evprop);
  if (spanner_type == SCM_EOL)
    spanner_type = get_property (ctxprop);
  return spanner_type;
}

void
New_dynamic_engraver::process_music ()
{
  if (current_spanner_
      && (accepted_spanevents_drul_[STOP]
	  || script_event_
	  || accepted_spanevents_drul_[START]))
    {
      Stream_event *ender = accepted_spanevents_drul_[STOP];
      if (!ender)
	ender = script_event_;

      if (!ender)
	ender = accepted_spanevents_drul_[START];

      finished_spanner_ = current_spanner_;
      announce_end_grob (finished_spanner_, ender->self_scm ());
      current_spanner_ = 0;
      current_span_event_ = 0;
    }

  if (accepted_spanevents_drul_[START])
    {
      current_span_event_ = accepted_spanevents_drul_[START];

      string start_type = get_spanner_type (current_span_event_);
      SCM cresc_type = get_property_setting (current_span_event_, "span-type",
					     (start_type + "Spanner").c_str ());

      if (cresc_type == ly_symbol2scm ("text"))
	{
	  current_spanner_
	    = make_spanner ("DynamicTextSpanner",
			    accepted_spanevents_drul_[START]->self_scm ());

	  SCM text = get_property_setting (current_span_event_, "span-text",
					   (start_type + "Text").c_str ());
	  if (Text_interface::is_markup (text))
	    current_spanner_->set_property ("text", text);
	}
      else
	{
	  if (cresc_type != ly_symbol2scm ("hairpin"))
	    {
	      string as_string = ly_scm_write_string (cresc_type);
	      current_span_event_
		->origin()->warning (_f ("unknown crescendo style: %s\ndefaulting to hairpin.", as_string.c_str()));
	    }
	  current_spanner_ = make_spanner ("Hairpin",
					   current_span_event_->self_scm ());
	}
      if (finished_spanner_)
	{
	  if (Hairpin::has_interface (finished_spanner_))
	    Pointer_group_interface::add_grob (finished_spanner_,
					       ly_symbol2scm ("adjacent-spanners"),
					       current_spanner_);
	  if (Hairpin::has_interface (current_spanner_))
	    Pointer_group_interface::add_grob (current_spanner_,
					       ly_symbol2scm ("adjacent-spanners"),
					       finished_spanner_);
	}
    }

  if (script_event_)
    {
      script_ = make_item ("DynamicText", script_event_->self_scm ());
      script_->set_property ("text",
			     script_event_->get_property ("text"));

      if (finished_spanner_)
	finished_spanner_->set_bound (RIGHT, script_);
      if (current_spanner_)
	current_spanner_->set_bound (LEFT, script_);
    }
}

void
New_dynamic_engraver::stop_translation_timestep ()
{
  if (finished_spanner_ && !finished_spanner_->get_bound (RIGHT))
    finished_spanner_
      ->set_bound (RIGHT,
		   unsmob_grob (get_property ("currentMusicalColumn")));

  if (current_spanner_ && !current_spanner_->get_bound (LEFT))
    current_spanner_
      ->set_bound (LEFT,
		   unsmob_grob (get_property ("currentMusicalColumn")));
  script_ = 0;
  script_event_ = 0;
  accepted_spanevents_drul_.set (0, 0);
  finished_spanner_ = 0;
}

void
New_dynamic_engraver::finalize ()
{
  if (current_spanner_
      && !current_spanner_->is_live ())
    current_spanner_ = 0;
  if (current_spanner_)
    {
      current_span_event_
	->origin ()->warning (_f ("unterminated %s",
				  get_spanner_type (current_span_event_)
				  .c_str ()));
      current_spanner_->suicide ();
      current_spanner_ = 0;
    }
}

string
New_dynamic_engraver::get_spanner_type (Stream_event *ev)
{
  string type;
  SCM start_sym = ev->get_property ("class");

  if (start_sym == ly_symbol2scm ("decrescendo-event"))
    type = "decrescendo";
  else if (start_sym == ly_symbol2scm ("crescendo-event"))
    type = "crescendo";
  else
    programming_error ("unknown dynamic spanner type");

  return type;
}

void
New_dynamic_engraver::acknowledge_note_column (Grob_info info)
{
  if (script_ && !script_->get_parent (X_AXIS))
    {
      extract_grob_set (info.grob (), "note-heads", heads);
      if (heads.size ())
	{
	  Grob *head = heads[0];
	  script_->set_parent (head, X_AXIS);
	  Self_alignment_interface::set_center_parent (script_, X_AXIS);
	}
    }

  if (current_spanner_ && !current_spanner_->get_bound (LEFT))
    current_spanner_->set_bound (LEFT, info.grob ());
  if (finished_spanner_ && !finished_spanner_->get_bound (RIGHT))
    finished_spanner_->set_bound (RIGHT, info.grob ());
}

ADD_ACKNOWLEDGER (New_dynamic_engraver, note_column);
ADD_TRANSLATOR (New_dynamic_engraver,
		/* doc */
		"Create hairpins, dynamic texts, and their vertical"
		" alignments.  The symbols are collected onto a"
		" @code{DynamicLineSpanner} grob which takes care of vertical"
		" positioning.",

		/* create */
		"DynamicTextSpanner "
		"DynamicText "
		"Hairpin ",

		/* read */
		"crescendoSpanner "
		"crescendoText "
		"currentMusicalColumn "
		"decrescendoSpanner "
		"decrescendoText ",

		/* write */
		""
		);
