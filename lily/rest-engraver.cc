/*
  rest-grav.cc -- implement Rest_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "staff-symbol-referencer.hh"
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
Rest_engraver::do_process_music ()
{
  if (rest_req_l_ && !rest_p_) 
    {
      rest_p_ = new Rest (get_property ("basicRestProperties"));
      Staff_symbol_referencer_interface si (rest_p_);
      si.set_interface ();
      
      rest_p_->set_elt_property ("duration-log",
				 gh_int2scm (rest_req_l_->duration_.durlog_i_)); 
      
      if (rest_req_l_->duration_.dots_i_)
	{
	  dot_p_ = new Dots (get_property ("basicDotsProperties"));

	  Staff_symbol_referencer_interface si (dot_p_);
	  si.set_interface ();
	  
	  rest_p_->add_dots (dot_p_);
	  dot_p_->set_elt_property ("dot-count",
				    gh_int2scm (rest_req_l_->duration_.dots_i_));
	  announce_element (Score_element_info (dot_p_,0));
	}

      announce_element (Score_element_info (rest_p_, rest_req_l_));
    }
}

bool
Rest_engraver::do_try_music (Music *req)
{
  if (Rest_req *r = dynamic_cast <Rest_req *> (req))
    {
      rest_req_l_ = r;
      return true;
    }  
  return false;
}


ADD_THIS_TRANSLATOR(Rest_engraver);
