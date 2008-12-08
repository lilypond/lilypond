/* 
  new-dynamic-engraver.cc -- implement New_dynamic_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2008 Han-Wen Nienhuys <hanwen@lilypond.org>
  
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
private:
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


void
New_dynamic_engraver::process_music ()
{
  if (current_spanner_
      && (accepted_spanevents_drul_[STOP] || script_event_ || accepted_spanevents_drul_[START]))
    {
      Stream_event* ender = accepted_spanevents_drul_[STOP];
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
      
      SCM start_sym = current_span_event_->get_property ("class");
      string start_type;
	  
      if (start_sym == ly_symbol2scm ("decrescendo-event"))
	start_type = "decrescendo";
      else if (start_sym == ly_symbol2scm ("crescendo-event"))
	start_type = "crescendo";
      else
	{
	  programming_error ("unknown dynamic spanner type");
	  return;
	}
      
      SCM cresc_type = get_property ((start_type + "Spanner").c_str ());

      if (cresc_type == ly_symbol2scm ("text"))
	{
	  current_spanner_
	    = make_spanner ("DynamicTextSpanner",
			    accepted_spanevents_drul_[START]->self_scm ());

	  SCM text = get_property ((start_type + "Text").c_str ());
	  if (Text_interface::is_markup (text))
	    {
	      current_spanner_->set_property ("text", text);
	    }
	}
      else
	{
	  if (cresc_type != ly_symbol2scm ("hairpin"))
	    {
	      // Fixme: should put value in error message.
	      string as_string = ly_scm_write_string (cresc_type);
	      current_span_event_
		->origin()->warning (_f ("unknown crescendo style: %s\ndefaulting to hairpin.", as_string.c_str()));
	    }
	  current_spanner_ = make_spanner ("Hairpin",
					   current_span_event_->self_scm ());
	  if (finished_spanner_)
	    {
	      Pointer_group_interface::add_grob (finished_spanner_,
						 ly_symbol2scm ("adjacent-hairpins"),
						 current_spanner_);

	      Pointer_group_interface::add_grob (current_spanner_,
						 ly_symbol2scm ("adjacent-hairpins"),
						 finished_spanner_);
	    }
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
	{
	  current_spanner_->set_bound (LEFT, script_);

	  if (!Hairpin::has_interface (current_spanner_))
	    set_nested_property (current_spanner_,
				 scm_list_3 (ly_symbol2scm ("bound-details"),
					     ly_symbol2scm ("left"),
					     ly_symbol2scm ("attach-dir")
					     ),
				 scm_from_int (RIGHT));

	}
    }
}



void
New_dynamic_engraver::stop_translation_timestep ()
{
  if (finished_spanner_ && !finished_spanner_->get_bound (RIGHT))
    finished_spanner_->set_bound (RIGHT,
				  unsmob_grob (get_property ("currentMusicalColumn")));

  if (current_spanner_ && !current_spanner_->get_bound (LEFT))
    current_spanner_->set_bound (LEFT,
				 unsmob_grob (get_property ("currentMusicalColumn")));
  script_ = 0;
  script_event_ = 0;
  accepted_spanevents_drul_.set (0, 0);
  finished_spanner_ = 0;
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
		"Hairpin "
		"TextSpanner ",

		/* read */
		"currentMusicalColumn ",

		/* write */
		""
		);
