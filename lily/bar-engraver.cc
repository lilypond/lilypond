/*
  bar-engraver.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997, 1998, 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "bar-engraver.hh"
#include "bar.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "command-request.hh"
#include "time-description.hh"
#include "engraver-group.hh"
#include "repeated-music.hh"

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
Bar_engraver::acknowledge_element (Score_element_info i)
{
  if (Bar *b = dynamic_cast<Bar *> (i.elem_l_))
    {
      // only bar-engraver should create bars
      assert (0);
    }
}

void
Bar_engraver::create_bar ()
{
  if (!bar_p_)
    {
      bar_p_ = new Bar;
      bar_p_->break_priority_i_  = 0;
      // urg: "" != empty...
      String default_type = get_property ("defaultBarType", 0);
      if (default_type.length_i ())
	{
	  bar_p_->type_str_ = default_type;
	}
      announce_element (Score_element_info (bar_p_, bar_req_l_));
    }
}

void
Bar_engraver::request_bar (String type_str)
{
  create_bar ();
  if (((type_str == "|:") && (bar_p_->type_str_ == ":|"))
    || ((type_str == ":|") && (bar_p_->type_str_ == "|:")))
    bar_p_->type_str_ = ":|:";
  else
    bar_p_->type_str_ = type_str;
}

void 
Bar_engraver::do_creation_processing ()
{
  create_bar ();
  bar_p_->type_str_ = "";
  Scalar prop = get_property ("barAuto", 0);
  auto_create_bar_b_ = prop.to_bool ();
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
  else if (!now_moment ())
    {
      create_bar ();
      bar_p_->type_str_ = "";
    }
  else 
    {
      Scalar always = get_property ("barAlways", 0);
      if ((time && !time->whole_in_measure_) || always.to_bool ())
	{
	  if (auto_create_bar_b_)
	    create_bar ();
	  Scalar prop = get_property ("barAuto", 0);
	  auto_create_bar_b_ = prop.to_bool ();
	}
    }
  
  if (!bar_p_)
    {
      Break_req r;
      r.penalty_i_ = Break_req::DISALLOW;
      daddy_grav_l ()->try_music (&r);
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


