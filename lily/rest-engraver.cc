/*
  rest-grav.cc -- implement Rest_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "rest-engraver.hh"
#include "musical-request.hh"
#include "dots.hh"
#include "rest.hh"
/*
  Should merge with Note_head_engraver
 */
Rest_engraver::Rest_engraver ()
{
  rest_req_l_ =0;
  rest_p_ =0;
  dot_p_ =0;
}

void
Rest_engraver::do_post_move_processing ()
{
  rest_req_l_ =0;
}

void
Rest_engraver::do_pre_move_processing ()
{
  if (rest_p_)
    {
      typeset_element (rest_p_);
      rest_p_ =0;
    }
  if (dot_p_)
    {
      typeset_element (dot_p_);
      dot_p_ =0;
    }    
}

void
Rest_engraver::do_process_requests ()
{
  if (rest_req_l_ && !rest_p_) 
    {
      rest_p_ = new Rest;
      rest_p_->balltype_i_ = rest_req_l_->duration_.durlog_i_; 
      rest_p_->dots_i_ = rest_req_l_->duration_.dots_i_;
      if (rest_p_->dots_i_)
	{
	  dot_p_ = new Dots;
	  rest_p_->dots_l_  =dot_p_;
	  announce_element (Score_element_info (dot_p_,0));
	}
      announce_element (Score_element_info (rest_p_, rest_req_l_));
    }
}

bool
Rest_engraver::do_try_request (Request *r)
{
  Musical_req *m = r->access_Musical_req ();
  if (!m || !m->access_Rest_req ())
    return false;

  rest_req_l_ = m->access_Rest_req ();	// ugh
  return true;
}

IMPLEMENT_IS_TYPE_B1(Rest_engraver, Engraver);
ADD_THIS_TRANSLATOR(Rest_engraver);
