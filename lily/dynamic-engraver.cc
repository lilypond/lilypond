/*
  dynamic-engraver.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "dimensions.hh"
#include "crescendo.hh"
#include "musical-request.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "paper-column.hh"
#include "staff-symbol.hh"
#include "note-column.hh"
#include "text-item.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "stem.hh"
#include "note-head.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "staff-symbol-referencer.hh"
#include "translator-group.hh"

class Dynamic_line_spanner : public Spanner
{
public:
  Dynamic_line_spanner ();
  VIRTUAL_COPY_CONS(Score_element);
  void add_column (Item*);
  Direction get_default_dir () const;
};

Dynamic_line_spanner::Dynamic_line_spanner ()
{
  set_elt_property ("transparent", SCM_BOOL_T);
  side_position (this).set_axis (Y_AXIS);
}

void
Dynamic_line_spanner::add_column (Item* n)
{
  if (!get_bound (LEFT))
    set_bound (LEFT, n);
  else
    set_bound (RIGHT, n);

  add_dependency (n);
}

Direction
Dynamic_line_spanner::get_default_dir () const
{
  return DOWN;
}

/**
   print text & hairpin dynamics.
 */
class Dynamic_engraver : public Engraver
{
  Text_item * text_p_;
  Crescendo * finished_cresc_p_;
  Crescendo * cresc_p_;

  Text_script_req* text_req_l_;
  Span_req * span_start_req_l_;
  Drul_array<Span_req*> span_req_l_drul_;

  Dynamic_line_spanner* line_spanner_;
  Moment last_request_mom_;
  
  void  typeset_all ();

public:
  VIRTUAL_COPY_CONS(Translator);
  Dynamic_engraver ();
  
protected:
  void announce_element (Score_element_info);
  
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
};

ADD_THIS_TRANSLATOR (Dynamic_engraver);

void
Dynamic_engraver::announce_element (Score_element_info i)
{
  group (i.elem_l_, "interfaces").add_thing (ly_symbol2scm ("dynamic"));
  
  Engraver::announce_element (i);
}


Dynamic_engraver::Dynamic_engraver ()
{
  text_p_ = 0;
  finished_cresc_p_ = 0;
  line_spanner_ = 0;
  span_start_req_l_ = 0;
  cresc_p_ =0;

  text_req_l_ = 0;
  span_req_l_drul_[START] = 0;
  span_req_l_drul_[STOP] = 0;
}

void
Dynamic_engraver::do_post_move_processing ()
{
  text_req_l_ = 0;
  span_req_l_drul_[START] = 0;
  span_req_l_drul_[STOP] = 0;

  /* ugr; we must attach the Dynamic_line_spanner to something
     to be sure that the linebreaker will not be confused
  */
  // if (line_spanner_)
  // line_spanner_->add_column (LEFT, get_staff_info ().command_pcol_l ());
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
	  span_req_l_drul_[s->span_dir_] = s;
	  return true;
	}
    }
  return false;
}

void
Dynamic_engraver::do_process_music ()
{
  if ((span_req_l_drul_[START] || text_req_l_) && !line_spanner_)
    {
      line_spanner_ = new Dynamic_line_spanner;
      side_position (line_spanner_).set_axis (Y_AXIS);
      announce_element (Score_element_info
			(line_spanner_,
			 text_req_l_ ? text_req_l_ : span_req_l_drul_[START]));

    }
	  
  if (span_req_l_drul_[START] || text_req_l_)
    last_request_mom_ = now_mom ();
  
  if (text_req_l_)
    {
      String loud = text_req_l_->text_str_;

      text_p_ = new Text_item;
      text_p_->set_elt_property ("text",
					  ly_str02scm (loud.ch_C ()));
      text_p_->set_elt_property ("style", gh_str02scm ("dynamic"));
      text_p_->set_elt_property ("script-priority",
					  gh_int2scm (100));
	  
      assert (line_spanner_);
      text_p_->set_parent (line_spanner_, Y_AXIS);
      announce_element (Score_element_info (text_p_, text_req_l_));
    }

  if (span_req_l_drul_[STOP])
    {
      if (!cresc_p_)
	{
	  span_req_l_drul_[STOP]->warning
	    (_ ("can't find start of (de)crescendo"));
	}
      else
	{
	  assert (!finished_cresc_p_);
	  cresc_p_->set_bound(RIGHT, get_staff_info ().musical_pcol_l ());
	  //	  cresc_p_->add_dependency (get_staff_info ().musical_pcol_l ());
	  finished_cresc_p_ = cresc_p_;
	  cresc_p_ = 0;
	  span_start_req_l_ = 0;
	}
    }

  if (span_req_l_drul_[START])
    {
      if (span_start_req_l_)
	{
	  span_req_l_drul_[START]->warning
	    (span_start_req_l_->span_dir_ == 1
	     ?
	     _ ("already have a crescendo")
	     : _ ("already have a decrescendo"));
	}
      else
	{
	  span_start_req_l_ = span_req_l_drul_[START];
	  cresc_p_  = new Crescendo;
	  cresc_p_->set_elt_property
	    ("grow-direction",
	     gh_int2scm ((span_req_l_drul_[START]->span_type_str_ == "crescendo")
			 ? BIGGER : SMALLER));
	      
	  SCM s = get_property (span_req_l_drul_[START]->span_type_str_ + "Text");
	  if (gh_string_p (s))
	    {
	      cresc_p_->set_elt_property ("start-text", s);
	      daddy_trans_l_->set_property (span_req_l_drul_[START]->span_type_str_
					    + "Text", SCM_UNDEFINED);
	    }

	  s = get_property (span_req_l_drul_[START]->span_type_str_ + "Spanner");
	  if (gh_string_p (s)) //&& ly_scm2string (s) != "hairpin")
	    {
	      cresc_p_->set_elt_property ("spanner", s);
	      daddy_trans_l_->set_property (span_req_l_drul_[START]->span_type_str_
					    + "Spanner", SCM_UNDEFINED);
	    }

	  cresc_p_->set_bound(LEFT, get_staff_info ().musical_pcol_l ());


	  // cresc_p_->add_dependency (get_staff_info ().musical_pcol_l ());

	  /* 
	      We know how wide the text is, if we can be sure that the
	      text already has relevant pointers into the paperdef,
	      and it has its font-size property set.

	      Since font-size may be set by a context higher up, we
	      can not be sure of the size.
	  */

	     
	  if (text_p_)
	    {
	      index_set_cell (cresc_p_->get_elt_property ("dynamic-drul"),
			      LEFT, SCM_BOOL_T);
	      if (finished_cresc_p_)
		index_set_cell (finished_cresc_p_->get_elt_property ("dynamic-drul"),
				RIGHT, SCM_BOOL_T);
	    }

	  assert (line_spanner_);
	  cresc_p_->set_parent (line_spanner_, Y_AXIS);
	  // cresc_p_->add_dependency (line_spanner_);
	  announce_element (Score_element_info (cresc_p_, span_req_l_drul_[START]));
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
  if (cresc_p_)
    {
      typeset_element (cresc_p_ );
      span_start_req_l_->warning (_ ("unterminated (de)crescendo"));
      cresc_p_ =0;
    }
  typeset_all ();
  if (line_spanner_)
    {
      typeset_element (line_spanner_);
      line_spanner_ = 0;
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
  
  if (text_p_)
    {
      typeset_element (text_p_);
      text_p_ = 0;
    }

  /*
    TODO: This should be optionised:
      * break when group of dynamic requests ends
      * break now 
      * continue through piece */
  if (line_spanner_ && last_request_mom_ < now_mom ())
    {

      side_position (line_spanner_).add_staff_support ();
      
      typeset_element (line_spanner_);
      line_spanner_ = 0;
    }
}

void
Dynamic_engraver::acknowledge_element (Score_element_info i)
{
  if (line_spanner_)
    {
      if (Note_column* n = dynamic_cast<Note_column*> (i.elem_l_))
	{
	  side_position (line_spanner_).add_support (n);
	  line_spanner_->add_column (n);
	}
    }
}
