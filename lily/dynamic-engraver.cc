/*
  dynamic-engraver.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "warn.hh"
#include "dimensions.hh"
#include "hairpin.hh"
#include "event.hh"
#include "paper-column.hh"
#include "note-column.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "translator-group.hh"
#include "axis-group-interface.hh"
#include "script.hh"

/*
  TODO:

  * direction of text-dynamic-event if not equal to direction of
  line-spanner

  - TODO: this engraver is too complicated. We should split it into
  the handling of the basic grobs and the  linespanner

  - TODO: the line-spanner is not killed after the (de)crescs are
  finished.

*/

/**
   print text & hairpin dynamics.
 */
class Dynamic_engraver : public Engraver
{
  Item * script_;
  Spanner * finished_cresc_;
  Spanner * cresc_;

  Music* script_ev_;
  
  Music * current_cresc_ev_;
  Drul_array<Music*> accepted_spanreqs_drul_;

  Spanner* line_spanner_;
  Spanner* finished_line_spanner_;

  Link_array<Note_column> pending_columns_;
  Link_array<Grob> pending_elements_;
  
  void typeset_all ();

TRANSLATOR_DECLARATIONS(Dynamic_engraver );
  
protected:
  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music *req);
  virtual void stop_translation_timestep ();
  virtual void process_music ();  
  virtual void start_translation_timestep ();
};




Dynamic_engraver::Dynamic_engraver ()
{
  script_ = 0;
  finished_cresc_ = 0;
  line_spanner_ = 0;
  finished_line_spanner_ = 0;
  current_cresc_ev_ = 0;
  cresc_ =0;

  script_ev_ = 0;
  accepted_spanreqs_drul_[START] = 0;
  accepted_spanreqs_drul_[STOP] = 0;
}

void
Dynamic_engraver::start_translation_timestep ()
{
  script_ev_ = 0;
  accepted_spanreqs_drul_[START] = 0;
  accepted_spanreqs_drul_[STOP] = 0;
}

bool
Dynamic_engraver::try_music (Music * m)
{
  if (m->is_mus_type ("absolute-dynamic-event"))
    {
      /*
	TODO: probably broken.
      */
      script_ev_ = m;
      return true;
    }
  else if (m->is_mus_type ("abort-event"))
    {
      accepted_spanreqs_drul_[LEFT] = 0;
      accepted_spanreqs_drul_[RIGHT] = 0;
      /*
	Let's not kill the line spanner, since that would fuck up
	earlier, not-to-be-terminated stuff.

	It will disappear by itself when stop_translation_timestep
	() finds that there is nothing to support anymore.  */
	  
      if (cresc_)
	cresc_->suicide ();
      cresc_ = 0;
    }
  else if (m->is_mus_type ("decrescendo-event")
	   || m->is_mus_type ("crescendo-event"))
    {
      Direction d = to_dir (m->get_mus_property ("span-direction"));
      accepted_spanreqs_drul_[d] = m;
      return true;
    }
  return false;
}

void
Dynamic_engraver::process_music ()
{
  if (accepted_spanreqs_drul_[START] || accepted_spanreqs_drul_[STOP] || script_ev_)
    {
      if (!line_spanner_)
	{
	  line_spanner_ = new Spanner (get_property ("DynamicLineSpanner"));

	  Music * rq = accepted_spanreqs_drul_[START];
	  if (script_ev_)
	    rq =  script_ev_ ;
	  announce_grob(line_spanner_, rq ? rq->self_scm(): SCM_EOL);
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
      script_ = new Item (get_property ("DynamicText"));
      script_->set_grob_property ("text",
				   script_ev_->get_mus_property ("text"));

      
      if (Direction d = to_dir (script_ev_->get_mus_property ("direction")))
	Directional_element_interface::set (line_spanner_, d);

      Axis_group_interface::add_element (line_spanner_, script_);

      announce_grob(script_, script_ev_->self_scm());
    }

  Music *stop_ev = accepted_spanreqs_drul_ [STOP] ?
    accepted_spanreqs_drul_[STOP] : script_ev_;

  if (accepted_spanreqs_drul_[STOP] || script_ev_)
    {
      /*
	finish side position alignment if the (de)cresc ends here, and
	there are no new dynamics.
       */


      if (cresc_)
	{
	  assert (!finished_cresc_ && cresc_);

	  cresc_->set_bound (RIGHT, script_
			       ? script_
			       : unsmob_grob (get_property ("currentMusicalColumn")));
	  add_bound_item (line_spanner_, cresc_->get_bound (RIGHT));
	  

	  finished_cresc_ = cresc_;
	  cresc_ = 0;
	  current_cresc_ev_ = 0;
	}
      else if (accepted_spanreqs_drul_[STOP] )
	{
	  accepted_spanreqs_drul_[STOP]->origin ()->warning(_ ("can't find start of (de)crescendo"));
	  stop_ev = 0;
	}
      
    }
  
  if (accepted_spanreqs_drul_[START])
    {
      if (current_cresc_ev_)
	{
	  Direction sd = to_dir (current_cresc_ev_->get_mus_property ("span-direction"));
	  String msg = sd == 1
	    ? _ ("already have a crescendo")
	    : _ ("already have a decrescendo");
      
	  accepted_spanreqs_drul_[START]->origin ()->warning (msg);
	  current_cresc_ev_->origin ()->warning (_("Cresc started here"));
	}
      else
	{
	  current_cresc_ev_ = accepted_spanreqs_drul_[START];

	  if (Direction d = to_dir (current_cresc_ev_->get_mus_property ("direction")))
	    Directional_element_interface::set (line_spanner_, d);

	  /*
	    TODO: Use symbols.
	  */

	  String start_type = 
	    ly_symbol2string (current_cresc_ev_->get_mus_property ("name"));

	  /*
	    ugh. Use push/pop?
	  */
	  if (start_type == "DecrescendoEvent")
	    start_type = "decrescendo";
	  else if (start_type == "CrescendoEvent")
	    start_type = "crescendo";
	  
	  SCM s = get_property ((start_type + "Spanner").to_str0 ());
	  if (!gh_symbol_p (s) || s == ly_symbol2scm ("hairpin"))
	    {
	      cresc_  = new Spanner (get_property ("Hairpin"));
	      cresc_->set_grob_property ("grow-direction",
					   gh_int2scm ((start_type == "crescendo")
						       ? BIGGER : SMALLER));
	      
	    }

	  
	  /*
	    This is a convenient (and legacy) interface to TextSpanners
	    for use in (de)crescendi.
	    Hmm.
	  */
	  else
	    {
	      cresc_  = new Spanner (get_property ("TextSpanner"));
	      cresc_->set_grob_property ("type", s);
	      daddy_trans_->set_property ((start_type
					    + "Spanner").to_str0 (), SCM_EOL);
	      s = get_property ((start_type + "Text").to_str0 ());
	      /*
		FIXME: use get_markup () to check type.
	      */
	      if (gh_string_p (s) || gh_pair_p (s))
		{
		  cresc_->set_grob_property ("edge-text",
					       gh_cons (s, scm_makfrom0str ("")));
		  daddy_trans_->set_property ((start_type + "Text").to_str0 (),
						SCM_EOL);
		}
	    }

	  cresc_->set_bound (LEFT, script_
			       ? script_
			       : unsmob_grob (get_property ("currentMusicalColumn")));

	  Axis_group_interface::add_element (line_spanner_, cresc_);

	  add_bound_item (line_spanner_, cresc_->get_bound (LEFT));
	  
	  announce_grob(cresc_, accepted_spanreqs_drul_[START]->self_scm());
	}
    }
}

void
Dynamic_engraver::stop_translation_timestep ()
{
  typeset_all ();
  if (!current_cresc_ev_)
    {
      finished_line_spanner_ = line_spanner_;
      line_spanner_ =0;
      typeset_all ();
    }
}

void
Dynamic_engraver::finalize ()
{
  typeset_all ();
  
  if (line_spanner_
      && !line_spanner_->live())
    line_spanner_ = 0;
  if (line_spanner_)
    {
      finished_line_spanner_ = line_spanner_;
      typeset_all ();
    }

  if (cresc_
      && !cresc_->live())
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
  /*
    remove suicided spanners,
    ugh: we'll need this for every spanner, beam, slur
    Hmm, how to do this, cleanly?
    Maybe just check at typeset_grob ()?
  */
  if (finished_cresc_
      && !finished_cresc_->live())
    finished_cresc_ = 0;
  if (finished_line_spanner_
      && !finished_line_spanner_->live())
    finished_line_spanner_ = 0;

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
      typeset_grob (finished_cresc_);
      finished_cresc_ =0;
    }
  
  if (script_)
    {
      typeset_grob (script_);
      script_ = 0;
    }
  if (finished_line_spanner_)
    {
      /* To make sure that this works */
      Side_position_interface::add_staff_support (finished_line_spanner_);
      
      /*
	We used to have
	
	     extend-spanner-over-elements (finished_line_spanner_);

	but this is rather kludgy, since finished_line_spanner_
	typically has a staff-symbol field set , extending it over the
	entire staff.

      */

      Grob * l = finished_line_spanner_->get_bound (LEFT );
      Grob * r = finished_line_spanner_->get_bound (RIGHT);      
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
	  Grob * cc = unsmob_grob (get_property ("currentMusicalColumn"));
	  Item * ci = dynamic_cast<Item*>(cc);
	  finished_line_spanner_->set_bound (RIGHT, ci);
	  finished_line_spanner_->set_bound (LEFT, ci);	  
	}
	
      typeset_grob (finished_line_spanner_);
      finished_line_spanner_ = 0;
    }
}

void
Dynamic_engraver::acknowledge_grob (Grob_info i)
{
  if (!line_spanner_)
    return ;
  
  if (Note_column::has_interface (i.grob_))
    {
      if (line_spanner_
	  /* Don't refill killed spanner */
	  && line_spanner_->live())
	{
	  Side_position_interface::add_support (line_spanner_,i.grob_);
	  add_bound_item (line_spanner_,dynamic_cast<Item*> (i.grob_));
	}

      if (script_ && !script_->get_parent (X_AXIS))
	{
	  script_->set_parent (i.grob_,  X_AXIS);
	}
      
    }
  else if (Script_interface::has_interface (i.grob_) && script_)
    {
      SCM p = i.grob_->get_grob_property ("script-priority");

      /*
	UGH.

	DynamicText doesn't really have a script-priority field.
       */
      if (gh_number_p (p)
	  && gh_scm2int (p) < gh_scm2int (script_->get_grob_property ("script-priority")))
	{
	  Side_position_interface::add_support (line_spanner_, i.grob_);

	}	  
    }
}
ENTER_DESCRIPTION(Dynamic_engraver,
/* descr */       "
This engraver creates hairpins, dynamic texts, and their vertical
alignments.  The symbols are collected onto a DynamicLineSpanner grob
which takes care of vertical positioning.  
",
		  
/* creats*/       "DynamicLineSpanner DynamicText Hairpin TextSpanner",
/* accepts */     "absolute-dynamic-event crescendo-event decrescendo-event",
/* acks  */      "note-column-interface script-interface",
/* reads */       "",
/* write */       "");
