/*
  dynamic-engraver.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "dimensions.hh"
#include "crescendo.hh"
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
  Link_array<Score_element> pending_element_arr_;
  
  void typeset_all ();

public:
  VIRTUAL_COPY_CONS(Translator);
  Dynamic_engraver ();
  
protected:
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
};

ADD_THIS_TRANSLATOR (Dynamic_engraver);


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
Dynamic_engraver::do_post_move_processing ()
{
  script_req_l_ = 0;
  accepted_spanreqs_drul_[START] = 0;
  accepted_spanreqs_drul_[STOP] = 0;
}

bool
Dynamic_engraver::do_try_music (Music * m)
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
	  if (line_spanner_)
	    line_spanner_->suicide ();
	  line_spanner_ = 0;
	  if (cresc_p_)
	    cresc_p_->suicide ();
	  cresc_p_ = 0;
	}
      else if (t == "crescendo"
	   || t == "decrescendo")
	{
	  accepted_spanreqs_drul_[s->get_span_dir()] = s;
	  return true;
	}
    }
  return false;
}

void
Dynamic_engraver::do_process_music ()
{
  if (accepted_spanreqs_drul_[START] || accepted_spanreqs_drul_[STOP] || script_req_l_)
    
    {
      if (!line_spanner_)
	{
	  line_spanner_ = new Spanner (get_property ("DynamicLineSpanner"));

	  Side_position::set_axis (line_spanner_, Y_AXIS);
	  Axis_group_interface::set_interface (line_spanner_);
	  Axis_group_interface::set_axes (line_spanner_, Y_AXIS, Y_AXIS);

	  Music * rq = accepted_spanreqs_drul_[START];
	  if (script_req_l_)
	    rq =  script_req_l_ ;
	  announce_element (line_spanner_, rq);
			 

	}
    }

  /*
    finish side position alignment if the (de)cresc ends here, and
    there are no new dynamics.
    
   */
  else if (accepted_spanreqs_drul_[STOP]
	   && !accepted_spanreqs_drul_[START] && !script_req_l_)
    {
      finished_line_spanner_ = line_spanner_;
      line_spanner_ = 0;
    }

	/*
	todo: resurrect  dynamic{direction, padding,minimumspace}
	*/
	/*
	During a (de)crescendo, pending request will not be cleared,
	and a line-spanner will always be created, as \< \! are already
	two requests.

	Maybe always creating a line-spanner for a (de)crescendo (see
	below) is not a good idea:

	    a\< b\p \!c

	the \p will be centred on the line-spanner, and thus clash
	with the hairpin.  When axis-group code is in place, the \p
	should move below the hairpin, which is probably better?

	Urg, but line-spanner must always have at least same duration
	as (de)crecsendo, b.o. line-breaking.
	*/

  

  /*
    maybe we should leave dynamic texts to the text-engraver and
    simply acknowledge them?
  */
  if (script_req_l_)
    {
      script_p_ = new Item (get_property ("DynamicText"));
      script_p_->set_elt_property ("text",
				   script_req_l_->get_mus_property ("text"));
      if (Direction d = script_req_l_->get_direction ())
	Directional_element_interface::set (line_spanner_, d);

      Axis_group_interface::add_element (line_spanner_, script_p_);

      announce_element (script_p_, script_req_l_);
    }

  if (accepted_spanreqs_drul_[STOP])
    {
      if (!cresc_p_)
	{
	  accepted_spanreqs_drul_[STOP]->origin ()->warning
	    (_ ("can't find start of (de)crescendo"));
	}
      else
	{
	  assert (!finished_cresc_p_);
	  Score_element* cc = unsmob_element (get_property ("currentMusicalColumn"));
	  
	  cresc_p_->set_bound (RIGHT, cc);

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
	    (current_cresc_req_->get_span_dir() == 1
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
	  SCM s = get_property ((start_type + "Spanner").ch_C());
	  if (!gh_string_p (s) || ly_scm2string (s) == "hairpin")
	    {
	      cresc_p_  = new Spanner (get_property ("Crescendo"));
	      cresc_p_->set_elt_property ("grow-direction",
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
	      cresc_p_->set_elt_property ("type", s);
	      daddy_trans_l_->set_property (start_type
					    + "Spanner", SCM_UNDEFINED);
	      s = get_property ((start_type + "Text").ch_C());
	      if (gh_string_p (s))
		{
		  cresc_p_->set_elt_property ("edge-text",
					      gh_cons (s, ly_str02scm ("")));
		  daddy_trans_l_->set_property (start_type + "Text",
						SCM_UNDEFINED);
		}
	    }
	
	  Score_element *cc = unsmob_element (get_property ("currentMusicalColumn"));
	  cresc_p_->set_bound (LEFT, cc);

	  if (script_p_)
	    {
	      Side_position::set_direction (script_p_, LEFT);
	      Side_position::set_axis (script_p_, X_AXIS);
	      Side_position::add_support (script_p_, cresc_p_);
	    }

	  Axis_group_interface::add_element (line_spanner_, cresc_p_);
	  announce_element (cresc_p_, accepted_spanreqs_drul_[START]);
	}
    }
}

void
Dynamic_engraver::do_pre_move_processing ()
{
  typeset_all ();
}

void
Dynamic_engraver::do_removal_processing ()
{
  typeset_all ();
  if (line_spanner_)
    {
      finished_line_spanner_ = line_spanner_;
      typeset_all ();
    }

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
  if (finished_cresc_p_)
    {
      typeset_element (finished_cresc_p_);
      finished_cresc_p_ =0;
    }
  
  if (script_p_)
    {
      typeset_element (script_p_);
      script_p_ = 0;
    }
  if (finished_line_spanner_)
    {
      Side_position::add_staff_support (finished_line_spanner_);
      extend_spanner_over_elements (finished_line_spanner_);
      typeset_element (finished_line_spanner_);
      finished_line_spanner_ = 0;
    }
}

void
Dynamic_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_column::has_interface (i.elem_l_))
    {
      if (line_spanner_)
	{
	  Side_position::add_support (line_spanner_,i.elem_l_);
	  add_bound_item (line_spanner_,dynamic_cast<Item*>(i.elem_l_));
	}
    }
}
