/*
  item.cc -- implement Item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "p-score.hh"
#include "debug.hh"
#include "item.hh"
#include "p-col.hh"

Item::Item()
{
    pcol_l_ = 0;

    broken_to_a_[0]
	= broken_to_a_[1]=0;
}

IMPLEMENT_STATIC_NAME(Item);
IMPLEMENT_IS_TYPE_B1(Item, Score_elem);

void
Item::do_print() const
{
#ifndef NPRINT
    mtor << "(unknown)";
#endif
}


Real 
Item::hpos_f()const
{
    return pcol_l_->hpos_f_ + offset().x;
}



Line_of_score *
Item::line_l()const
{
    return pcol_l_->line_l_;
}

int
Item::break_status_i() const
{
    PCol * c = pcol_l_;
    if (c->breakable_b())
	return 0;
    else if (!c->daddy_l_) 
	return 0; 
    else if (c == c->daddy_l_->prebreak_p_)
	return -1;
    else 
	return 1;
}

void
Item::do_breakable_col_processing()
{
    PCol * c = pcol_l_;
    if (!c->breakable_b())
	return;
    
    for (int i=0; i < 2; i++) {
	broken_to_a_[i] = clone()->item();
	pscore_l_->typeset_item(broken_to_a_[i], c, -1+ 2*i);
	broken_to_a_[i]->handle_prebroken_dependencies();
    }

    handle_prebroken_dependencies();
}

Item*
Item::find_prebroken_piece(Line_of_score*l) const
{
    if (line_l() == l) 
	return this;
    else if (broken_to_a_[0] && broken_to_a_[0]->line_l() == l)
	return broken_to_a_[0];
    else if (broken_to_a_[1] && broken_to_a_[1]->line_l() == l)
	return broken_to_a_[1];

    return 0;
}

Item*
Item::find_prebroken_piece(PCol*c)const
{
    if (c == pcol_l_ )
	return (Item *) this;	// ugh

    if (c == pcol_l_->prebreak_p_)
	return (Item *) broken_to_a_[0];
    else if (c==pcol_l_->postbreak_p_)
	return  (Item *)broken_to_a_[1];

    assert(false);
}

void
Item::handle_prebroken_dependencies()
{
    if ( pcol_l_->breakable_b() || pcol_l_->daddy_l_ )
	Score_elem::handle_prebroken_dependencies();
}
