/*
  span-bar-grav.cc -- implement Span_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "span-bar.hh"
#include "span-bar-grav.hh"
#include "vertical-align-spanner.hh"

Span_bar_engraver::Span_bar_engraver()
{
    spanbar_p_ =0;
    valign_l_ =0;
}

void
Span_bar_engraver::acknowledge_element(Score_elem_info i)
{
    if ( i.elem_l_->is_type_b( Bar::static_name() ) ) {
	bar_l_arr_.push( (Bar*)i.elem_l_->item() );
	
	if (bar_l_arr_.size() >= 2 && !spanbar_p_) {
	    spanbar_p_ = new Span_bar;
	    announce_element( Score_elem_info(spanbar_p_,0) );
	}
    } else if  (i.elem_l_->is_type_b( Vertical_align_spanner::static_name() ) 
		&& i.origin_grav_l_arr_.size() <= 2) {
	valign_l_ = (Vertical_align_spanner*)i.elem_l_->spanner();
    }
}

void
Span_bar_engraver::do_pre_move_processing()
{
    if (spanbar_p_) {
	for (int i=0; i < bar_l_arr_.size() ; i++)
	    spanbar_p_->add(bar_l_arr_[i]);
	spanbar_p_->set( valign_l_ );
	typeset_breakable_item(spanbar_p_);
	spanbar_p_ =0;
    }
    bar_l_arr_.set_size( 0);
	
}

IMPLEMENT_STATIC_NAME(Span_bar_engraver);
IMPLEMENT_IS_TYPE_B1(Span_bar_engraver,Engraver);
ADD_THIS_ENGRAVER(Span_bar_engraver);
