/*
  dynamic-engraver.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "axis-group-interface.hh"
#include "context.hh"
#include "dimensions.hh"
#include "directional-element-interface.hh"
#include "engraver.hh"
#include "hairpin.hh"
#include "international.hh"
#include "interval.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "script-interface.hh"
#include "self-alignment-interface.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

#include "translator.icc"

/*
  TODO:

  * direction of text-dynamic-event if not equal to direction of
  line-spanner

  - TODO: this engraver is too complicated. We should split it into
  the handling of the basic grobs and the linespanner

  - TODO: the line-spanner is not killed after the (de)crescs are
  finished.
*/

/**
   print text & hairpin dynamics.
*/
class Dynamic_engraver : public Engraver
{
  Item *script_;
  Spanner *line_spanner_;
  Spanner *cresc_;

  Spanner *finished_line_spanner_;
  Spanner *finished_cresc_;

  Music *script_ev_;
  Music *current_cresc_ev_;

  Drul_array<Music *> accepted_spanevents_drul_;

  vector<Note_column*> pending_columns_;
  vector<Grob*> pending_elements_;

  void typeset_all ();

  TRANSLATOR_DECLARATIONS (Dynamic_engraver);
  DECLARE_ACKNOWLEDGER (accidental);
  DECLARE_ACKNOWLEDGER (script);
  DECLARE_ACKNOWLEDGER (stem_tremolo);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_ACKNOWLEDGER (slur);

protected:
  virtual void finalize ();
  virtual bool try_music (Music *event);
  void stop_translation_timestep ();
  void process_music ();
};

Dynamic_engraver::Dynamic_engraver ()
{
  script_ = 0;
  finished_cresc_ = 0;
  line_spanner_ = 0;
  finished_line_spanner_ = 0;
  current_cresc_ev_ = 0;
  cresc_ = 0;

  script_ev_ = 0;
  accepted_spanevents_drul_[START] = 0;
  accepted_spanevents_drul_[STOP] = 0;
}

bool
Dynamic_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("absolute-dynamic-event"))
    {
      /*
	TODO: probably broken.
      */
      script_ev_ = m;
      return true;
    }
  else if (m->is_mus_type ("decrescendo-event")
	   || m->is_mus_type ("crescendo-event"))
    {
      Direction d = to_dir (m->get_property ("span-direction"));

      accepted_spanevents_drul_[d] = m;
      if (current_cresc_ev_ && d == START)
	accepted_spanevents_drul_[STOP] = m;
      return true;
    }
  return false;
}

void
Dynamic_engraver::process_music ()
{
  if (accepted_spanevents_drul_[START] || accepted_spanevents_drul_[STOP] || script_ev_)
    {
      if (!line_spanner_)
	{
	  Music *rq = accepted_spanevents_drul_[START];
	  line_spanner_ = make_spanner ("DynamicLineSpanner", rq ? rq->self_scm () : SCM_EOL);
	  if (script_ev_)
	    rq = script_ev_;
	}
    }

  /*
    During a (de)crescendo, pending event will not be cleared,
    and a line-spanner will always be created, as \< \! are already
    two events.

    Note: line-spanner must always have at least same duration
    as (de)crecsendo, b.o. line-breaking.
  */

  /*
    maybe we should leave dynamic texts to the text-engraver and
    simply acknowledge them?
  */
  if (script_ev_)
    {
      script_ = make_item ("DynamicText", script_ev_->self_scm ());
      script_->set_property ("text",
			     script_ev_->get_property ("text"));

      if (Direction d = to_dir (script_ev_->get_property ("direction")))
	set_grob_direction (line_spanner_, d);
      else if (Direction d = to_dir (line_spanner_->get_property ("direction")))
	set_grob_direction (script_, d);

      Axis_group_interface::add_element (line_spanner_, script_);
    }

  Music *stop_ev = accepted_spanevents_drul_ [STOP]
    ? accepted_spanevents_drul_[STOP] : script_ev_;

  if (accepted_spanevents_drul_[STOP] || script_ev_)
    {
      /*
	finish side position alignment if the (de)cresc ends here, and
	there are no new dynamics.
      */

      if (cresc_)
	{
	  assert (!finished_cresc_ && cresc_);

	  if (script_)
	    {
	      cresc_->set_bound (RIGHT, script_);
	      add_bound_item (line_spanner_, script_);
	    }

	  finished_cresc_ = cresc_;
	  cresc_ = 0;
	  current_cresc_ev_ = 0;
	}
      else if (accepted_spanevents_drul_[STOP])
	{
	  accepted_spanevents_drul_[STOP]->origin ()->warning (_ ("can't find start of (de)crescendo"));
	  stop_ev = 0;
	}
    }

  if (accepted_spanevents_drul_[START])
    {
      if (current_cresc_ev_)
	{
	  string msg = _ ("already have a decrescendo");
	  if (current_cresc_ev_->is_mus_type ("decrescendo-event"))
	    msg = _ ("already have a crescendo");

	  accepted_spanevents_drul_[START]->origin ()->warning (msg);
	  current_cresc_ev_->origin ()->warning (_ ("cresc starts here"));
	}
      else
	{
	  current_cresc_ev_ = accepted_spanevents_drul_[START];

	  if (Direction d = to_dir (current_cresc_ev_->get_property ("direction")))
	    set_grob_direction (line_spanner_, d);

	  /*
	    TODO: Use symbols.
	  */

	  string start_type
	    = ly_symbol2string (current_cresc_ev_->get_property ("name"));
	  
	  if (start_type == "DecrescendoEvent")
	    start_type = "decrescendo";
	  else if (start_type == "CrescendoEvent")
	    start_type = "crescendo";
	  
				       

	  /*
	    UGH. TODO: should read from original event, so appearance
	    may be altered with \tweak.
	   */
	  SCM s = get_property ((start_type + "Spanner").c_str ());
	  if (!scm_is_symbol (s) || s == ly_symbol2scm ("hairpin"))
	    {
	      cresc_ = make_spanner ("Hairpin", accepted_spanevents_drul_[START]->self_scm ());
	      if (finished_cresc_)
		{
		  Pointer_group_interface::add_grob (finished_cresc_,
						     ly_symbol2scm ("adjacent-hairpins"),
						     cresc_);

		  Pointer_group_interface::add_grob (cresc_,
						     ly_symbol2scm ("adjacent-hairpins"),
						     finished_cresc_);
		}
	      cresc_->set_property ("grow-direction",
				    scm_from_int ((start_type == "crescendo")
						  ? BIGGER : SMALLER));
	    }

	  /*
	    This is a convenient (and legacy) interface to TextSpanners
	    for use in (de)crescendi.
	    Hmm.
	  */
	  else
	    {
	      cresc_ = make_spanner ("DynamicTextSpanner", accepted_spanevents_drul_[START]->self_scm ());
	      cresc_->set_property ("style", s);
	      context ()->set_property ((start_type
					 + "Spanner").c_str (), SCM_EOL);
	      s = get_property ((start_type + "Text").c_str ());
	      /*
		FIXME: use get_markup () to check type.
	      */
	      if (scm_is_string (s) || scm_is_pair (s))
		{
		  cresc_->set_property ("edge-text",
					scm_cons (s, scm_makfrom0str ("")));
		  context ()->set_property ((start_type + "Text").c_str (),
					    SCM_EOL);
		}
	    }

	  if (script_)
	    {
	      cresc_->set_bound (LEFT, script_);
	      add_bound_item (line_spanner_, cresc_->get_bound (LEFT));
	    }

	  Axis_group_interface::add_element (line_spanner_, cresc_);
	}
    }
}

void
Dynamic_engraver::stop_translation_timestep ()
{
  if (!current_cresc_ev_ && line_spanner_)
    {
      assert (!finished_line_spanner_);
      finished_line_spanner_ = line_spanner_;
      line_spanner_ = 0;
    }

  typeset_all ();

  if (cresc_ && !cresc_->get_bound (LEFT))
    {
      cresc_->set_bound (LEFT, unsmob_grob (get_property ("currentMusicalColumn")));
      add_bound_item (line_spanner_, cresc_->get_bound (LEFT));
    }

  script_ev_ = 0;
  accepted_spanevents_drul_[START] = 0;
  accepted_spanevents_drul_[STOP] = 0;
}

void
Dynamic_engraver::finalize ()
{
  typeset_all ();

  if (line_spanner_
      && !line_spanner_->is_live ())
    line_spanner_ = 0;
  if (line_spanner_)
    {
      finished_line_spanner_ = line_spanner_;
      typeset_all ();
    }

  if (cresc_
      && !cresc_->is_live ())
    cresc_ = 0;
  if (cresc_)
    {
      current_cresc_ev_->origin ()->warning (_ ("unterminated (de)crescendo"));
      cresc_->suicide ();
      cresc_ = 0;
    }
}

void
Dynamic_engraver::typeset_all ()
{
  if (finished_cresc_)
    {
      if (!finished_cresc_->get_bound (RIGHT))
	{
	  finished_cresc_->set_bound (RIGHT, script_
				      ? script_
				      : unsmob_grob (get_property ("currentMusicalColumn")));

	  if (finished_line_spanner_)
	    add_bound_item (finished_line_spanner_,
			    finished_cresc_->get_bound (RIGHT));
	}
      finished_cresc_ = 0;
    }

  script_ = 0;
  if (finished_line_spanner_)
    {
      /*
	We used to have

	extend-spanner-over-elements (finished_line_spanner_);

	but this is rather kludgy, since finished_line_spanner_
	typically has a staff-symbol field set , extending it over the
	entire staff.

      */

      Grob *l = finished_line_spanner_->get_bound (LEFT);
      Grob *r = finished_line_spanner_->get_bound (RIGHT);
      if (!r && l)
	finished_line_spanner_->set_bound (RIGHT, l);
      else if (!l && r)
	finished_line_spanner_->set_bound (LEFT, r);
      else if (!r && !l)
	{
	  /*
	    This is a isolated dynamic apparently, and does not even have
	    any interesting support item.
	  */
	  Grob *cc = unsmob_grob (get_property ("currentMusicalColumn"));
	  Item *ci = dynamic_cast<Item *> (cc);
	  finished_line_spanner_->set_bound (RIGHT, ci);
	  finished_line_spanner_->set_bound (LEFT, ci);
	}

      finished_line_spanner_ = 0;
    }
}


void
Dynamic_engraver::acknowledge_accidental (Grob_info info)
{
  if (line_spanner_)
    Side_position_interface::add_support (line_spanner_, info.grob ());
}


void
Dynamic_engraver::acknowledge_stem_tremolo (Grob_info info)
{
  if (line_spanner_)
    Side_position_interface::add_support (line_spanner_, info.grob ());
}


void
Dynamic_engraver::acknowledge_slur (Grob_info info)
{
  if (line_spanner_)
    Side_position_interface::add_support (line_spanner_, info.grob ());
}


void
Dynamic_engraver::acknowledge_note_column (Grob_info info)
{
  if (!line_spanner_)
    return;

  if (line_spanner_
      /* Don't refill killed spanner */
      && line_spanner_->is_live ())
    {
      Side_position_interface::add_support (line_spanner_, info.grob ());
      add_bound_item (line_spanner_, dynamic_cast<Item *> (info.grob ()));
    }

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

  if (cresc_)
    {
      if (!cresc_->get_bound (LEFT))
	{
	  cresc_->set_bound (LEFT, info.grob ());
	  add_bound_item (line_spanner_, cresc_->get_bound (LEFT));
	}
    }

  if (finished_cresc_ && !finished_cresc_->get_bound (RIGHT))
    finished_cresc_->set_bound (RIGHT, info.grob ());
}

void
Dynamic_engraver::acknowledge_script (Grob_info info)
{
  if (!line_spanner_ || !script_)
    return;

  SCM p = info.grob ()->get_property ("script-priority");

  /*
    UGH.

    DynamicText doesn't really have a script-priority field.
  */
  if (scm_is_number (p)
      && scm_to_int (p)
      < scm_to_int (script_->get_property ("script-priority")))
    Side_position_interface::add_support (line_spanner_, info.grob ());
}

ADD_ACKNOWLEDGER (Dynamic_engraver, accidental);
ADD_ACKNOWLEDGER (Dynamic_engraver, script);
ADD_ACKNOWLEDGER (Dynamic_engraver, note_column);
ADD_ACKNOWLEDGER (Dynamic_engraver, slur);
ADD_ACKNOWLEDGER (Dynamic_engraver, stem_tremolo);

ADD_TRANSLATOR (Dynamic_engraver,
		/* doc */
		"This engraver creates hairpins, dynamic texts, and their vertical\n"
		"alignments.  The symbols are collected onto a DynamicLineSpanner grob\n"
		"which takes care of vertical positioning.  ",

		/* create */ "DynamicLineSpanner DynamicText Hairpin TextSpanner",
		/* accept */ "absolute-dynamic-event crescendo-event decrescendo-event",
		/* read */ "",
		/* write */ "");
