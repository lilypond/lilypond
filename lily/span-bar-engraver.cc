/*
  span-bar-grav.cc -- implement Span_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "span-bar.hh"
#include "span-bar-engraver.hh"
#include "vertical-align-spanner.hh"

Span_bar_engraver::Span_bar_engraver()
{
  spanbar_p_ =0;
  valign_l_ =0;
}

Span_bar*
Span_bar_engraver::get_span_bar_p() const
{
  return new Span_bar;
}


void
Span_bar_engraver::do_creation_processing ()
{
}

void
Span_bar_engraver::do_removal_processing ()
{
}    

void
Span_bar_engraver::acknowledge_element (Score_element_info i)
{
  int depth = i.origin_grav_l_arr_.size();
  if (depth > 1
      && i.elem_l_->is_type_b (Bar::static_name())) 
    {
      bar_l_arr_.push ((Bar*)dynamic_cast <Item *> (i.elem_l_));
	
      if (bar_l_arr_.size() >= 2 && !spanbar_p_) 
	/*
	  hmm, i do want a bracket with one staff some times, but not always
	  if (bar_l_arr_.size() >= 1 && !spanbar_p_)

	  --jcn
	*/

	/*

	  use a property?  get_property ("singleStaffBracket") ?

	  --hwn
	 */
	{
	  spanbar_p_ = get_span_bar_p();
	  announce_element (Score_element_info (spanbar_p_,0));
	  spanbar_p_-> type_str_ = bar_l_arr_[0]->type_str_;
	}
    }
  else if  (i.elem_l_->is_type_b (Vertical_align_spanner::static_name()) 
	    && i.origin_grav_l_arr_.size() <= 2) 
    {
      valign_l_ = (Vertical_align_spanner*)dynamic_cast <Spanner *> (i.elem_l_);
    }
}

void
Span_bar_engraver::do_pre_move_processing()
{
  if (spanbar_p_) 
    {
      for (int i=0; i < bar_l_arr_.size() ; i++)
	spanbar_p_->add_bar (bar_l_arr_[i]);
      spanbar_p_->set_align (valign_l_);
      typeset_element (spanbar_p_);
      spanbar_p_ =0;
    }
  bar_l_arr_.set_size (0);
}


IMPLEMENT_IS_TYPE_B1(Span_bar_engraver,Engraver);
ADD_THIS_TRANSLATOR(Span_bar_engraver);
