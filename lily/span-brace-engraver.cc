/*
  span-brace-engraver.cc -- implement Span_brace_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
	   Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "span-brace-item.hh"
#include "span-brace-engraver.hh"
#include "vertical-align-spanner.hh"

IMPLEMENT_STATIC_NAME(Span_brace_engraver);
IMPLEMENT_IS_TYPE_B1(Span_brace_engraver,Engraver);
ADD_THIS_ENGRAVER(Span_brace_engraver);

Span_brace_engraver::Span_brace_engraver()
{
    span_brace_p_ = 0;
    valign_l_ = 0;
}

Span_brace_engraver::~Span_brace_engraver()
{
}

void
Span_brace_engraver::acknowledge_element( Score_elem_info i )
{
    if ( i.elem_l_->is_type_b( Bar::static_name() ) ) {
	bar_l_arr_.push( (Bar*)i.elem_l_->item() );
	
	if ( bar_l_arr_.size() >= 2 && !span_brace_p_ ) {
	    span_brace_p_ = new Span_brace_item;
	    announce_element( Score_elem_info( span_brace_p_, 0 ) );
	}
    } 
    else if ( i.elem_l_->is_type_b( Vertical_align_spanner::static_name() ) 
	&& i.origin_grav_l_arr_.size() <= 2 )
	valign_l_ = (Vertical_align_spanner*)i.elem_l_->spanner();
}

void
Span_brace_engraver::do_pre_move_processing()
{
    if ( span_brace_p_ ) {
	for ( int i=0; i < bar_l_arr_.size() ; i++ )
	    span_brace_p_->add( bar_l_arr_[ i ] );
	span_brace_p_->set( valign_l_ );
	typeset_breakable_item( span_brace_p_ );
	span_brace_p_ = 0;
    }

    bar_l_arr_.clear();
}

