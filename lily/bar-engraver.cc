/*
  bar-engraver.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997, 1998, 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#include "score-engraver.hh"
#include "bar-engraver.hh"
#include "staff-bar.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "command-request.hh"
#include "time-description.hh"
#include "engraver-group-engraver.hh"
#include "warn.hh"

Bar_engraver::Bar_engraver()
{
  bar_p_ =0;
  do_post_move_processing();
}

bool
Bar_engraver::do_try_music (Music*r_l)
{
  if (Bar_req  * b= dynamic_cast <Bar_req *> (r_l))
    {
      if (bar_req_l_ && bar_req_l_->equal_b (b)) // huh?
	return false;
      
      bar_req_l_ = b;
      return true;
    }
  
  return false;
}



void
Bar_engraver::create_bar ()
{
  if (!bar_p_)
    {
      bar_p_ = new Staff_bar;
      bar_p_->set_elt_property (break_priority_scm_sym, gh_int2scm (0));

      // urg: "" != empty...
      String default_type = get_property ("defaultBarType", 0);
      if (default_type.length_i ())
	{
	  bar_p_->type_str_ = default_type;
	}

      /*
	urg.  Why did I implement this?
       */
      Scalar prop = get_property ("barAtLineStart", 0);
      if (prop.to_bool ())
	{
	  bar_p_->set_elt_property (at_line_start_scm_sym, SCM_BOOL_T);
	}
      prop = get_property ("barSize", 0);
      if (prop.isnum_b ())
	{
	  bar_p_->set_elt_property (bar_size_scm_sym, 
				    gh_double2scm (Real(prop)));
	}
      announce_element (Score_element_info (bar_p_, bar_req_l_));
    }
}

/**
   Make a barline.  If there are both |: and :| requested, merge them
   to :|:.

*/
void
Bar_engraver::request_bar (String requested_type)
{
  Scalar prop = get_property ("barAtLineStart", 0);
  if (!now_mom ())
    {
      Scalar prop = get_property ("barAtLineStart", 0);
      if (!prop.to_bool ())
	return;
    }
  bool  bar_existed = bar_p_;
  create_bar ();
  if (bar_existed && requested_type == "")
    {
      return;
    }
  else if (((requested_type == "|:") && (bar_p_->type_str_ == ":|"))
    || ((requested_type == ":|") && (bar_p_->type_str_ == "|:")))
    bar_p_->type_str_ = ":|:";
  else
    bar_p_->type_str_ = requested_type;
}

void 
Bar_engraver::do_creation_processing ()
{
  create_bar ();
  bar_p_->type_str_ = "";
}

void
Bar_engraver::do_removal_processing ()
{
  if (bar_p_) 
    {
      typeset_element (bar_p_);
      bar_p_ =0;
    }
}

void
Bar_engraver::do_process_requests()
{  
  Time_description const *time = get_staff_info().time_C_;
  if (bar_req_l_) 
    {
      create_bar ();    
      bar_p_->type_str_ = bar_req_l_->type_str_;
    }
  else if (!now_mom ())
    {
      create_bar ();
      bar_p_->type_str_ = "|";
    }
  else 
    {
      Scalar nonauto = get_property ("barNonAuto", 0);
      if (!nonauto.to_bool ())
	{
	  Scalar always = get_property ("barAlways", 0);
	  if ((time && !time->whole_in_measure_) || always.to_bool ())
	    create_bar ();
	}
    }
  
  if (!bar_p_)
    {
      Score_engraver * e = 0;
      Translator * t  =  daddy_grav_l ();
      for (; !e && t;  t = t->daddy_trans_l_)
	{
	  e = dynamic_cast<Score_engraver*> (t);
	}
      
      if (!e)
	programming_error ("No score engraver!");
      else
	e->forbid_breaks ();
    }
}


void 
Bar_engraver::do_pre_move_processing()
{
  if (bar_p_) 
    {
      typeset_element (bar_p_);
      bar_p_ =0;
    }
}

void
Bar_engraver::do_post_move_processing()
{
  bar_req_l_ = 0;
}



ADD_THIS_TRANSLATOR(Bar_engraver);


