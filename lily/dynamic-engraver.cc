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
  Item * text_p_;
  Spanner * finished_cresc_p_;
  Spanner * cresc_p_;

  Text_script_req* text_req_l_;
  
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
  text_p_ = 0;
  finished_cresc_p_ = 0;
  line_spanner_ = 0;
  finished_line_spanner_ = 0;
  current_cresc_req_ = 0;
  cresc_p_ =0;

  text_req_l_ = 0;
  accepted_spanreqs_drul_[START] = 0;
  accepted_spanreqs_drul_[STOP] = 0;
}

void
Dynamic_engraver::do_post_move_processing ()
{
  text_req_l_ = 0;
  accepted_spanreqs_drul_[START] = 0;
  accepted_spanreqs_drul_[STOP] = 0;
}

bool
Dynamic_engraver::do_try_music (Music * m)
{
  if (Text_script_req* d = dynamic_cast <Text_script_req*> (m))
    {
      if (d->style_str_ == "dynamic")
	{
	  text_req_l_ = d;
	  return true;
	}
    }
  else if (Span_req* s =  dynamic_cast <Span_req*> (m))
    {
      if ((s->span_type_str_ == "crescendo"
	   || s->span_type_str_ == "decrescendo"))
	{
	  accepted_spanreqs_drul_[s->span_dir_] = s;
	  return true;
	}
    }
  return false;
}

void
Dynamic_engraver::do_process_music ()
{
  if (accepted_spanreqs_drul_[START] || accepted_spanreqs_drul_[STOP] || text_req_l_)
    
    {
      if (!line_spanner_)
	{
	  line_spanner_ = new Spanner (get_property ("basicDynamicLineSpannerProperties"));

	  Side_position::set_axis (line_spanner_, Y_AXIS);
	  Axis_group_interface::set_interface (line_spanner_);
	  Axis_group_interface::set_axes (line_spanner_, Y_AXIS, Y_AXIS);
	  announce_element (line_spanner_,
			     text_req_l_ ? text_req_l_ : accepted_spanreqs_drul_[START]);

	}
    }

  /*
    TODO: should finish and create new spanner if vertical dyn-direction is changed.
   */
  else if (!accepted_spanreqs_drul_[START] && !text_req_l_)
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


  if (text_req_l_)
    {
      String loud = text_req_l_->text_str_;

      text_p_ = new Item (get_property ("basicDynamicTextProperties"));
      text_p_->set_elt_property ("text", ly_str02scm (loud.ch_C ()));
      if (Direction d=text_req_l_->get_direction ())
	Directional_element_interface (line_spanner_).set (d);

      Axis_group_interface::add_element (line_spanner_, text_p_);

      text_p_->add_offset_callback (Side_position::aligned_on_self,
				    Y_AXIS);
      announce_element (text_p_, text_req_l_);
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
	  cresc_p_->set_bound (RIGHT, unsmob_element (get_property ("currentMusicalColumn")));
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
	    (current_cresc_req_->span_dir_ == 1
	     ?
	     _ ("already have a crescendo")
	     : _ ("already have a decrescendo"));
	}
      else
	{
	  current_cresc_req_ = accepted_spanreqs_drul_[START];
	  cresc_p_  = new Spanner (get_property ("basicCrescendoProperties"));
	  Crescendo::set_interface (cresc_p_);
	  cresc_p_->set_elt_property
	    ("grow-direction",
	     gh_int2scm ((accepted_spanreqs_drul_[START]->span_type_str_ == "crescendo")
			 ? BIGGER : SMALLER));
	      
	  SCM s = get_property ((accepted_spanreqs_drul_[START]->span_type_str_ + "Text").ch_C());
	  if (gh_string_p (s))
	    {
	      cresc_p_->set_elt_property ("start-text", s);
	      daddy_trans_l_->set_property (accepted_spanreqs_drul_[START]->span_type_str_
					    + "Text", SCM_UNDEFINED);
	    }

	  s = get_property ((accepted_spanreqs_drul_[START]->span_type_str_ + "Spanner").ch_C());


	  /*
	    TODO: Use symbols.
	   */
	  if (gh_string_p (s)) //&& ly_scm2string (s) != "hairpin")
	    {
	      cresc_p_->set_elt_property ("spanner", s);
	      daddy_trans_l_->set_property (accepted_spanreqs_drul_[START]->span_type_str_
					    + "Spanner", SCM_UNDEFINED);
	    }

	  cresc_p_->set_bound (LEFT, unsmob_element (get_property ("currentMusicalColumn")));


	  /* 
	      We know how wide the text is, if we can be sure that the
	      text already has relevant pointers into the paperdef,
	      and it has its font-size property set.

	      Since font-size may be set by a context higher up, we
	      can not be sure of the size.


	      We shouldn't try to do this stuff here, the Item should
	      do it when the score is finished.  We could maybe
	      set a callback to have the Item do the alignment if
	      it is not a special symbol, like Crescendo.
	  */

	  
	  if (text_p_)
	    {
	      index_set_cell (cresc_p_->get_elt_property ("dynamic-drul"),
			      LEFT, text_p_->self_scm ());
	      if (finished_cresc_p_)
		index_set_cell (finished_cresc_p_->get_elt_property ("dynamic-drul"),
				RIGHT, text_p_->self_scm ());
	    }

	  Axis_group_interface::add_element (line_spanner_, cresc_p_);
	  cresc_p_->set_elt_property ("self-alignment-Y", gh_int2scm (0));
	  cresc_p_->add_offset_callback
	    (Side_position::aligned_on_self, Y_AXIS);
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
  
  if (cresc_p_)
    {
      typeset_element (cresc_p_ );
      finished_cresc_p_ = cresc_p_;
      current_cresc_req_->origin ()->warning (_ ("unterminated (de)crescendo"));
    }
  if (line_spanner_)
    {
      finished_line_spanner_ = line_spanner_;
    }
  typeset_all ();
}

void
Dynamic_engraver::typeset_all ()
{  
  if (finished_cresc_p_)
    {
      typeset_element (finished_cresc_p_);
      finished_cresc_p_ =0;
    }
  
  if (text_p_)
    {
      typeset_element (text_p_);
      text_p_ = 0;
    }
  if (finished_line_spanner_)
    {
      Side_position::add_staff_support (finished_line_spanner_);

      if (!finished_line_spanner_->get_bound (LEFT))
	{
	  Score_element * cmc
	    = unsmob_element (get_property ("currentMusicalColumn"));
	  finished_line_spanner_->set_bound (LEFT, cmc);
	}
      if (!finished_line_spanner_->get_bound (RIGHT))
	finished_line_spanner_->set_bound (RIGHT,
					   finished_line_spanner_->get_bound (LEFT));
      
      
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
