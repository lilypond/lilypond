/*
  span-bar-grav.cc -- implement Base_span_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
#include "lily-guile.hh"
#include "span-bar.hh"
#include "base-span-bar-engraver.hh"
#include "axis-align-spanner.hh"

Base_span_bar_engraver::Base_span_bar_engraver()
{
  spanbar_p_ =0;
  use_priority_b_ = true;
}

Span_bar*
Base_span_bar_engraver::get_span_bar_p() const
{
  return new Span_bar;
}


void
Base_span_bar_engraver::acknowledge_element (Score_element_info i)
{
  int depth = i.origin_trans_l_arr (this).size();
  if (depth > 1
      && dynamic_cast<Bar *> (i.elem_l_)) 
    {
      bar_l_arr_.push (dynamic_cast<Bar *> (i.elem_l_));

      if (bar_l_arr_.size() >= 2 && !spanbar_p_) 
	/*
	  hmm, i do want a bracket with one staff some times, but not always
	  if (bar_l_arr_.size() >= 1 && !spanbar_p_)

	  --jcn
	*/

	/*

	  use a property?  get_property ("singleStaffBracket"); ?

	  --hwn
	 */
	{
	  spanbar_p_ = get_span_bar_p();
	  spanbar_p_->set_parent (bar_l_arr_[0], Y_AXIS);

	  /*
	    UGH. UGH UUHGK GUHG G
	    (ly_eval_str ??)
	   */
	  String visnam =  String(name()) + "-visibility";
	  
	  spanbar_p_->set_elt_property ("visibility-lambda",
					ly_eval_str (visnam.ch_C()));

	  if (use_priority_b_)
	    {
	      spanbar_p_->set_elt_property ("break-aligned", SCM_BOOL_T);
	    }
	  else
	    {
	      spanbar_p_->set_parent (bar_l_arr_[0], X_AXIS);
	    }
	  
	  announce_element (Score_element_info (spanbar_p_,0));
	  if (!gh_string_p (spanbar_p_->get_elt_property ("glyph")))
	    spanbar_p_-> set_elt_property ("glyph",
					   bar_l_arr_[0]->get_elt_property ("glyph"));
	}
    }
}

void
Base_span_bar_engraver::do_pre_move_processing()
{
  if (spanbar_p_) 
    {
      for (int i=0; i < bar_l_arr_.size() ; i++)
	spanbar_p_->add_bar (bar_l_arr_[i]);
      typeset_element (spanbar_p_);
      spanbar_p_ =0;
    }
  bar_l_arr_.set_size (0);
}



ADD_THIS_TRANSLATOR(Base_span_bar_engraver);

