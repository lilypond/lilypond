/*
  bar-reg.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
Bar_engraver::create_bar ()
{
  if (!bar_p_)
    {
      bar_p_ = new Bar;
      bar_p_->break_priority_i_  = 0;
      String default_type = get_property ("defaultBarType");
      if (default_type.length_i ())
	{
	  bar_p_->type_str_ = default_type;
	}
      announce_element (Score_element_info (bar_p_, bar_req_l_));
    }
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
  if (bar_req_l_) 
    {
      if (!bar_p_)
	create_bar ();    

      bar_p_->type_str_ = bar_req_l_->type_str_;
    }
  else 
    {
      Time_description const *time = get_staff_info().time_C_;
      String always = get_property ("barAlways");
      if ((time && !time->whole_in_measure_) || always.length_i ()) 
 	create_bar ();
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


