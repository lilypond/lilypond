/*
  dynamic-engraver.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "dimensions.hh"
#include "hairpin.hh"
#include "musical-request.hh"
#include "paper-column.hh"
#include "note-column.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "translator-group.hh"
#include "axis-group-interface.hh"


/*
  TODO:

  * direction of text-dynamic-request if not equal to direction of
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
  Item * script_p_;
  Spanner * finished_cresc_p_;
  Spanner * cresc_p_;

  Text_script_req* script_req_l_;
  
  Span_req * current_cresc_req_;
  Drul_array<Span_req*> accepted_spanreqs_drul_;

  Spanner* line_spanner_;
  Spanner* finished_line_spanner_;

  Link_array<Note_column> pending_column_arr_;
  Link_array<Grob> pending_element_arr_;
  
  void typeset_all ();

TRANSLATOR_DECLARATIONS(Dynamic_engraver );
  
protected:
  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music *req_l);
  virtual void stop_translation_timestep ();
  virtual void process_music ();  
  virtual void start_translation_timestep ();
};




Dynamic_engraver::Dynamic_engraver ()
{
  script_p_ = 0;
  finished_cresc_p_ = 0;
  line_spanner_ = 0;
  finished_line_spanner_ = 0;
  current_cresc_req_ = 0;
  cresc_p_ =0;

  script_req_l_ = 0;
  accepted_spanreqs_drul_[START] = 0;
  accepted_spanreqs_drul_[STOP] = 0;
}

void
Dynamic_engraver::start_translation_timestep ()
{
  script_req_l_ = 0;
  accepted_spanreqs_drul_[START] = 0;
  accepted_spanreqs_drul_[STOP] = 0;
}

bool
Dynamic_engraver::try_music (Music * m)
{
  if (dynamic_cast <Text_script_req*> (m)
      && m->get_mus_property ("text-type") == ly_symbol2scm ("dynamic"))
    {
      script_req_l_ = dynamic_cast<Text_script_req*> (m);
      return true;
    }
  else if (Span_req* s =  dynamic_cast <Span_req*> (m))
    {
      String t = ly_scm2string (s->get_mus_property ("span-type"));
      if (t== "abort")
	{
	  accepted_spanreqs_drul_[LEFT] = 0;
	  accepted_spanreqs_drul_[RIGHT] = 0;
	  /*
	    Let's not kill the line spanner, since that would fuck up
	    earlier, not-to-be-terminated stuff.

	    It will disappear by itself when stop_translation_timestep
 () finds that there is nothing to support anymore.  */
	  
	  if (cresc_p_)
	    cresc_p_->suicide ();
	  cresc_p_ = 0;
	}
      else if (t == "crescendo"
	   || t == "decrescendo")
	{
	  accepted_spanreqs_drul_[s->get_span_dir ()] = s;
	  return true;
	}
    }
  return false;
}

void
Dynamic_engraver::process_music ()
{
  if (accepted_spanreqs_drul_[START] || accepted_spanreqs_drul_[STOP] || script_req_l_)
    {
      if (!line_spanner_)
	{
	  line_spanner_ = new Spanner (get_property ("DynamicLineSpanner"));

	  Side_position_interface::set_axis (line_spanner_, Y_AXIS);
	  Axis_group_interface::set_interface (line_spanner_);
	  Axis_group_interface::set_axes (line_spanner_, Y_AXIS, Y_AXIS);

	  Music * rq = accepted_spanreqs_drul_[START];
	  if (script_req_l_)
	    rq =  script_req_l_ ;
	  announce_grob (line_spanner_, rq);
			 

	}
    }
  
	/*
	During a (de)crescendo, pending request will not be cleared,
	and a line-spanner will always be created, as \< \! are already
	two requests.

	Note: line-spanner must always have at least same duration
	as (de)crecsendo, b.o. line-breaking.
	*/

  

  /*
    maybe we should leave dynamic texts to the text-engraver and
    simply acknowledge them?
  */
  if (script_req_l_)
    {
      script_p_ = new Item (get_property ("DynamicText"));
      script_p_->set_grob_property ("text",
				   script_req_l_->get_mus_property ("text"));
      
      Side_position_interface::set_direction (script_p_, DOWN);

      if (Direction d = script_req_l_->get_direction ())
	Directional_element_interface::set (line_spanner_, d);

      Axis_group_interface::add_element (line_spanner_, script_p_);

      announce_grob (script_p_, script_req_l_);
    }

  if (accepted_spanreqs_drul_[STOP])
    {
      /*
	finish side position alignment if the (de)cresc ends here, and
	there are no new dynamics.
       */
 
      if (!cresc_p_)
	{
	  accepted_spanreqs_drul_[STOP]->origin ()->warning
 (_ ("can't find start of (de)crescendo"));
	  accepted_spanreqs_drul_[STOP] = 0;
	}
      else
	{
	  assert (!finished_cresc_p_ && cresc_p_);

	  cresc_p_->set_bound (RIGHT, script_p_
			       ? script_p_
			       : unsmob_grob (get_property ("currentMusicalColumn")));
	  add_bound_item (line_spanner_, cresc_p_->get_bound (RIGHT));
	  

	  finished_cresc_p_ = cresc_p_;
	  cresc_p_ = 0;
	  current_cresc_req_ = 0;
	}
    }
  
  if (accepted_spanreqs_drul_[START])
    {
      if (current_cresc_req_)
	{
	  accepted_spanreqs_drul_[START]->origin ()->warning
 (current_cresc_req_->get_span_dir () == 1
	     ? _ ("already have a crescendo")
	     : _ ("already have a decrescendo"));
	}
      else
	{
	  current_cresc_req_ = accepted_spanreqs_drul_[START];

	  /*
	    TODO: Use symbols.
	  */

	  String start_type = ly_scm2string (accepted_spanreqs_drul_[START]->get_mus_property ("span-type"));

	  /*
	    ugh. Use push/pop?
	  */
	  SCM s = get_property ((start_type + "Spanner").ch_C ());
	  if (!gh_symbol_p (s) || s == ly_symbol2scm ("hairpin"))
	    {
	      cresc_p_  = new Spanner (get_property ("Hairpin"));
	      cresc_p_->set_grob_property ("grow-direction",
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
	      cresc_p_  = new Spanner (get_property ("TextSpanner"));
	      cresc_p_->set_interface (ly_symbol2scm ("dynamic-interface"));
	      cresc_p_->set_grob_property ("type", s);
	      
	      daddy_trans_l_->set_property ((start_type
					    + "Spanner").ch_C(), SCM_UNDEFINED);
	      s = get_property ((start_type + "Text").ch_C ());
	      /*
		FIXME: use markup_p () to check type.
	      */
	      if (gh_string_p (s) || gh_pair_p (s))
		{
		  cresc_p_->set_grob_property ("edge-text",
					       gh_cons (s, ly_str02scm ("")));
		  daddy_trans_l_->set_property ((start_type + "Text").ch_C(),
						SCM_UNDEFINED);
		}
	    }

	  cresc_p_->set_bound (LEFT, script_p_
			       ? script_p_
			       : unsmob_grob (get_property ("currentMusicalColumn")));

	  Axis_group_interface::add_element (line_spanner_, cresc_p_);

	  add_bound_item (line_spanner_, cresc_p_->get_bound (LEFT));
	  
	  announce_grob (cresc_p_, accepted_spanreqs_drul_[START]);
	}
    }
}

void
Dynamic_engraver::stop_translation_timestep ()
{
  typeset_all ();
  if (script_req_l_ && !current_cresc_req_)
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
      && line_spanner_->immutable_property_alist_ == SCM_EOL)
    line_spanner_ = 0;
  if (line_spanner_)
    {
      finished_line_spanner_ = line_spanner_;
      typeset_all ();
    }

  if (cresc_p_
      && cresc_p_->immutable_property_alist_ == SCM_EOL)
    cresc_p_ = 0;
  if (cresc_p_)
    {
      current_cresc_req_->origin ()->warning (_ ("unterminated (de)crescendo"));
      cresc_p_->suicide ();
      cresc_p_ = 0;
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
  if (finished_cresc_p_
      && finished_cresc_p_->immutable_property_alist_ == SCM_EOL)
    finished_cresc_p_ = 0;
  if (finished_line_spanner_
      && finished_line_spanner_->immutable_property_alist_ == SCM_EOL)
    finished_line_spanner_ = 0;

  if (finished_cresc_p_)
    {
      if (!finished_cresc_p_->get_bound (RIGHT))
	{
	  finished_cresc_p_->set_bound (RIGHT, script_p_
					? script_p_
					: unsmob_grob (get_property ("currentMusicalColumn")));

	  if (finished_line_spanner_)
	    add_bound_item (finished_line_spanner_,
			    finished_cresc_p_->get_bound (RIGHT));
	}
      typeset_grob (finished_cresc_p_);
      finished_cresc_p_ =0;
    }
  
  if (script_p_)
    {
      typeset_grob (script_p_);
      script_p_ = 0;
    }
  if (finished_line_spanner_)
    {
      /* To make sure that this works */
      Side_position_interface::add_staff_support (finished_line_spanner_);
      
      /*
	We used to have
	
	     extend_spanner_over_elements (finished_line_spanner_);

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
  if (Note_column::has_interface (i.grob_l_))
    {
      if (line_spanner_
	  /* Don't refill killed spanner */
	  && line_spanner_->immutable_property_alist_ != SCM_EOL)
	{
	  Side_position_interface::add_support (line_spanner_,i.grob_l_);
	  add_bound_item (line_spanner_,dynamic_cast<Item*> (i.grob_l_));
	}
    }
}
ENTER_DESCRIPTION(Dynamic_engraver,
/* descr */       "",
/* creats*/       "DynamicLineSpanner DynamicText Hairpin TextSpanner",
/* acks  */       "note-column-interface",
/* reads */       "",
/* write */       "");
