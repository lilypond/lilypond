/*
  rest-grav.cc -- implement Rest_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "musical-request.hh"
#include "dots.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"


class Rest_engraver : public Engraver
{
  Rest_req *rest_req_l_;
  Item * dot_p_;
  Score_element* rest_p_;
protected:
  virtual bool do_try_music (Music *);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_process_music ();
public:
  
  VIRTUAL_COPY_CONS(Translator);
  Rest_engraver ();
};


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
      rest_p_ = new Item (get_property ("basicRestProperties"));
      Rhythmic_head::set_interface (rest_p_);
      Staff_symbol_referencer::set_interface (rest_p_);

      
      rest_p_->set_elt_property ("duration-log",
				 gh_int2scm (rest_req_l_->duration_.durlog_i_)); 
      
      if (rest_req_l_->duration_.dots_i_)
	{
	  dot_p_ = new Item (get_property ("basicDotsProperties"));

	  Staff_symbol_referencer::set_interface (dot_p_);
	  Rhythmic_head::set_dots (rest_p_, dot_p_);
	  dot_p_->set_parent (rest_p_, Y_AXIS);
	  dot_p_->add_offset_callback (Dots::quantised_position_callback, Y_AXIS);
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
