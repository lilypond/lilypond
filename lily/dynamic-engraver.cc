/*
  dynamic-engraver.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "dimensions.hh"
#include "dimension-cache.hh"
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
#include "axis-group-interface.hh"


/*
  TODO:

  * direction of text-dynamic-request if not equalt to direction
  of line-spanner

  * FIXME: this has gotten a bit too hairy.
 */

class Dynamic_line_spanner : public Spanner
{
public:
  Dynamic_line_spanner ();
  VIRTUAL_COPY_CONS(Score_element);
  void add_column (Note_column*);
  void add_element (Score_element*);
};

Dynamic_line_spanner::Dynamic_line_spanner ()
{
  set_elt_property ("transparent", SCM_BOOL_T);
  side_position (this).set_axis (Y_AXIS);
  Axis_group_interface (this).set_interface ();
  Axis_group_interface (this).set_axes (X_AXIS, Y_AXIS);
}

void
Dynamic_line_spanner::add_column (Note_column* n)
{
  if (!get_bound (LEFT))
    set_bound (LEFT, n);
  else
    set_bound (RIGHT, n);

  add_dependency (n);
}


void
Dynamic_line_spanner::add_element (Score_element* e)
{
  e->set_parent (this, Y_AXIS);
  Axis_group_interface (this).add_element (e); 
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
  Dynamic_line_spanner* finished_line_spanner_;
  Moment last_request_mom_;

  Array<Note_column*> pending_column_arr_;
  Link_array<Score_element> pending_element_arr_;
  
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
  finished_line_spanner_ = 0;
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
  if ((span_req_l_drul_[START] || span_req_l_drul_[STOP] || text_req_l_)
      && !line_spanner_
      && pending_element_arr_.size ())
    {
      line_spanner_ = new Dynamic_line_spanner;
      for (int i = 0; i < pending_column_arr_.size (); i++)
	line_spanner_->add_column (pending_column_arr_[i]);
      pending_column_arr_.clear ();
      announce_element (Score_element_info
			(line_spanner_,
			 text_req_l_ ? text_req_l_ : span_req_l_drul_[START]));

    }

  if (line_spanner_ && pending_element_arr_.size ())
    {
      for (int i = 0; i < pending_element_arr_.size (); i++)
	line_spanner_->add_element (pending_element_arr_[i]);
      pending_element_arr_.clear ();
    }

  /*
    TODO: This should be optionised:
      * break when group of dynamic requests ends
      * break now  (only if no cresc. in progress)
      * continue through piece */
  if (span_req_l_drul_[START] || span_req_l_drul_[STOP] || text_req_l_)
    {
      last_request_mom_ = now_mom ();
    }
  else
    {
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
      if (now_mom () > last_request_mom_ && !span_start_req_l_)
	{
	  for (int i = 0; i < pending_element_arr_.size (); i++)
	    {
	      Score_element* e = pending_element_arr_[i];
	      side_position (e).set_axis (Y_AXIS);
	      side_position (e).add_staff_support ();

	      /*
		UGH UGH 
	      */
	      Direction d = directional_element (e).get ();
	      if (!d)
		{
		  SCM s = get_property ("dynamicDirection");
		  if (!isdir_b (s))
		    s = get_property ("verticalDirection");
		  if (isdir_b (s))
		    d = to_dir (s);
		  directional_element (e).set (d);
		}
	  
	      SCM s = get_property ("dynamicPadding");
	      if (gh_number_p (s))
		e->set_elt_property ("padding", s);
	      s = get_property ("dynamicMinimumSpace");
	      if (gh_number_p (s))
		e->set_elt_property ("minimum-space", s);
	    }
	  pending_element_arr_.clear ();
	  if (line_spanner_)
	    {
	      for (int i = 0; i < pending_column_arr_.size (); i++)
		line_spanner_->add_column (pending_column_arr_[i]);
	      pending_column_arr_.clear ();
	      finished_line_spanner_ = line_spanner_;
	      line_spanner_ = 0;
	    }
	}
    } 

  if (text_req_l_)
    {
      String loud = text_req_l_->text_str_;

      text_p_ = new Text_item;
      text_p_->set_elt_property ("text",
					  ly_str02scm (loud.ch_C ()));
      text_p_->set_elt_property ("style", gh_str02scm ("dynamic"));
      text_p_->set_elt_property ("script-priority",
					  gh_int2scm (100));
      if (Direction d=text_req_l_->get_direction ())
	directional_element (text_p_).set (d);
      pending_element_arr_.push (text_p_);
      text_p_->set_elt_property ("self-alignment-Y", gh_int2scm (0));
      text_p_->add_offset_callback (Side_position_interface::aligned_on_self,
				    Y_AXIS);
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
	  cresc_p_->set_bound (RIGHT, get_staff_info ().musical_pcol_l ());
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


	  /*
	    TODO: Use symbols.
	   */
	  if (gh_string_p (s)) //&& ly_scm2string (s) != "hairpin")
	    {
	      cresc_p_->set_elt_property ("spanner", s);
	      daddy_trans_l_->set_property (span_req_l_drul_[START]->span_type_str_
					    + "Spanner", SCM_UNDEFINED);
	    }

	  cresc_p_->set_bound (LEFT, get_staff_info ().musical_pcol_l ());


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
			      LEFT, SCM_BOOL_T);
	      if (finished_cresc_p_)
		index_set_cell (finished_cresc_p_->get_elt_property ("dynamic-drul"),
				RIGHT, SCM_BOOL_T);
	    }
	  pending_element_arr_.push (cresc_p_);
	  cresc_p_->set_elt_property ("self-alignment-Y", gh_int2scm (0));
	  cresc_p_->add_offset_callback
	    (Side_position_interface::aligned_on_self, Y_AXIS);
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
      side_position (line_spanner_).add_staff_support ();
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
  if (finished_line_spanner_)
    {
      side_position (finished_line_spanner_).add_staff_support ();
      typeset_element (finished_line_spanner_);
      finished_line_spanner_ = 0;
    }
}

void
Dynamic_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_column* n = dynamic_cast<Note_column*> (i.elem_l_))
    {
      if (line_spanner_)
	{
	  side_position (line_spanner_).add_support (n);
	  line_spanner_->add_column (n);
	}
      else
	{
	  pending_column_arr_.push (n);
	}
    }
}
