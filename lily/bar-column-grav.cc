/*
  bar-column-grav.cc -- implement Bar_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "bar-column-grav.hh"
#include "bar-column.hh"
#include "request.hh"
#include "script.hh"
#include "bar.hh"

Bar_column_engraver::Bar_column_engraver()
{
  bar_l_ =0;
  barcol_p_ =0;
}


void
Bar_column_engraver::acknowledge_element (Score_elem_info info)
{
  if (info.elem_l_->is_type_b (Script::static_name()) 
      && info.req_l_->command()
      && info.origin_grav_l_arr_.size() == 1) 
    {
      script_l_arr_.push ((Script*)info.elem_l_->item());
    }
  else if  (info.origin_grav_l_arr_.size() == 1 
	    && info.elem_l_->is_type_b (Bar::static_name()))
    bar_l_ = (Bar*)info.elem_l_->item();
	
  if (bar_l_ && !barcol_p_) 
    {
      barcol_p_ = new Bar_column;
      barcol_p_->breakable_b_ =true;
      barcol_p_->break_priority_i_ = bar_l_ -> break_priority_i_;
      barcol_p_->set_bar (bar_l_);
      
      announce_element (Score_elem_info (barcol_p_, 0));
    }

  if  (barcol_p_) 
    {
      for (int i=0; i < script_l_arr_.size(); i++) 
	{
	  script_l_arr_[i]->breakable_b_ = true;
	  script_l_arr_[i]->break_priority_i_ = barcol_p_-> break_priority_i_;
	  barcol_p_->add (script_l_arr_[i]);
	}
      script_l_arr_.clear();
    }
}

void
Bar_column_engraver::do_pre_move_processing()
{
  if (barcol_p_) 
    {
      typeset_element (barcol_p_);
      barcol_p_ =0;
    }
}

void
Bar_column_engraver::do_post_move_processing()
{
  script_l_arr_.clear();
  bar_l_ =0;
}

IMPLEMENT_IS_TYPE_B1(Bar_column_engraver, Engraver);
ADD_THIS_TRANSLATOR(Bar_column_engraver);
