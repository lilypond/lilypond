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

/*
 Wat mij betreft wel DYN_LINE
 */
#define DYN_LINE


#ifdef DYN_LINE
class Dynamic_line_spanner : public Spanner
{
public:
  Dynamic_line_spanner ();
  
  void add_column (Note_column*);
  Direction get_default_dir () const;
protected:
  virtual void do_post_processing ();
};

Dynamic_line_spanner::Dynamic_line_spanner ()
{
  set_elt_property ("transparent", SCM_BOOL_T);
  side_position (this).set_axis (Y_AXIS);
}

void
Dynamic_line_spanner::add_column (Note_column* n)
{
  if (!spanned_drul_[LEFT])
    set_bounds (LEFT, n);
  set_bounds (RIGHT, n);

  add_dependency (n);
}

Direction
Dynamic_line_spanner::get_default_dir () const
{
  return DOWN;
}

void
Dynamic_line_spanner::do_post_processing ()
{
  Spanner::do_post_processing ();
  Direction dir = directional_element (this).get ();
  if (!dir)
    dir = get_default_dir ();

  /*
    Hier is ook vast iets voor?
   */
  Staff_symbol_referencer_interface si (this);
  Real above_staff = si.line_count () + 2;

#if 0
  // Aargh, nu snap ik waarom ik het niet snap
  // zie Staff_symbol_referencer_interface::set_position 

  if (si.position_f () * dir < above_staff)
    si.set_position (above_staff * (int)dir);

  SCM s = get_elt_property ("padding");
  if (gh_number_p (s))
    {
      si.set_position (si.position_f () + gh_scm2double (s) * (int) dir);
    }
#else
  Real dy = 0;
  Real pos = si.position_f () * dir;
  if (pos * dir < above_staff)
    dy = above_staff;

  SCM s = get_elt_property ("padding");
  if (gh_number_p (s))
    dy += gh_scm2double (s);
  
  Real half_space = si.staff_space () / 2;
  translate_axis (dy*half_space*dir, Y_AXIS);
#endif
  
}

#endif
/*
  TODO:
    Baseline alignment / character metrics of dynamic symbols.
 */

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

#ifdef DYN_LINE
  Dynamic_line_spanner* line_spanner_;
#else
  Spanner* line_spanner_;
#endif
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
  virtual void do_process_requests ();
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
Dynamic_engraver::do_process_requests ()
{
  if ((span_req_l_drul_[START] || text_req_l_) && !line_spanner_)
    {
#ifdef DYN_LINE
      line_spanner_ = new Dynamic_line_spanner;
#else
      line_spanner_ = new Spanner;
      line_spanner_->set_elt_property ("transparent", SCM_BOOL_T);
      side_position (line_spanner_).set_axis (Y_AXIS);
#endif
      announce_element (Score_element_info
			(line_spanner_,
			 text_req_l_ ? text_req_l_ : span_req_l_drul_[START]));

    }
	  
  if (span_req_l_drul_[START] || text_req_l_)
    last_request_mom_ = now_mom ();
  
#ifndef DYN_LINE
  if (line_spanner_)
    {
      /*
	Generic property will handle this for a Dynamic_line_spanner
       */
      Direction dir = DOWN;
      SCM s = get_property ("dynamicDirection");
      if (!isdir_b (s))
	{
	  s = get_property ("verticalDirection");
	}
      
      if (isdir_b (s) && to_dir (s))
	dir = to_dir (s);
      
      line_spanner_->set_elt_property ("direction", gh_int2scm ((int)dir));

      s = get_property ("dynamicPadding");
      Real padding;
      if (gh_number_p (s))
	padding = gh_scm2double (s);
      else
	padding = 2;
      line_spanner_->set_elt_property ("padding", gh_double2scm (padding));
    }
#endif 
  
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
	  cresc_p_->set_bounds(RIGHT, get_staff_info ().musical_pcol_l ());
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

	  cresc_p_->set_bounds(LEFT, get_staff_info ().musical_pcol_l ());
	  cresc_p_->set_bounds(RIGHT, get_staff_info ().musical_pcol_l ());

	  // arrragh, brr, urg: we know how wide text is, no?
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
#ifndef DYN_LINE
      Direction dir = directional_element (line_spanner_).get ();
      Real staff_space = Staff_symbol_referencer_interface (line_spanner_).staff_space ();
      SCM s = line_spanner_->get_elt_property ("padding");
      line_spanner_->translate_axis (gh_scm2double (s) * staff_space * (int)dir, Y_AXIS);
#endif
      typeset_element (line_spanner_);
      line_spanner_ = 0;
    }
}


void
Dynamic_engraver::typeset_all ()
{  
  if (finished_cresc_p_)
    {
      finished_cresc_p_->set_bounds (RIGHT, get_staff_info ().musical_pcol_l ());
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
      * continue through piece
   */
  if (line_spanner_ && last_request_mom_ < now_mom ())
    {
#ifndef DYN_LINE
      Direction dir = directional_element (line_spanner_).get ();
      Real staff_space = Staff_symbol_referencer_interface (line_spanner_).staff_space ();
      SCM s = line_spanner_->get_elt_property ("padding");
      line_spanner_->translate_axis (gh_scm2double (s) * staff_space * (int)dir, Y_AXIS);
#endif
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
#ifdef DYN_LINE
	  line_spanner_->add_column (n);
#else
	  if (!line_spanner_->spanned_drul_[LEFT])
	    line_spanner_->set_bounds (LEFT, n);
	  line_spanner_->set_bounds (RIGHT, n);
	  
	  line_spanner_->add_dependency (n);
#endif
	}
    }
}
