/*
  item.cc -- implement Item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-score.hh"
#include "debug.hh"
#include "item.hh"
#include "p-col.hh"
#include "elem-group.hh"

Item::Item()
{
    breakable_b_ = false;
    break_status_i_ = 0;
    pcol_l_ = 0;
    broken_to_a_[0] = broken_to_a_[1]=0;
}

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
    return pcol_l_->hpos_f_ + absolute_coordinate(X_AXIS);
}

Line_of_score *
Item::line_l()const
{
    return pcol_l_->line_l_;
}

int
Item::break_status_i() const
{
    return break_status_i_;
}

void
Item::copy_breakable_items()
{
    if ( broken_to_a_[0] || broken_to_a_[1] )
	return;
    Item *new_copies[2];
    for (int i=0; i < 2; i++) {
	Item * item_p = clone()->item();
	item_p->copy_dependencies(*this);
	
	item_p->break_status_i_ =  -1+ 2*i;
	pscore_l_->typeset_item(item_p, pcol_l_);
	item_p->handle_prebroken_dependencies();
	new_copies[i] =item_p;
    }
    broken_to_a_= new_copies;
}

void
Item::do_breakable_col_processing()
{
    if (!breakable_b_ || !pcol_l_->breakable_b())
	return;

    copy_breakable_items();
    handle_prebroken_dependencies();

    /*
      Otherwise the broken items won't be pre_process()'ed.
     */
    add_dependency( broken_to_a_[0] );
    add_dependency( broken_to_a_[1] );    

}

Item*
Item::find_prebroken_piece(Line_of_score*l) const
{
    if (line_l() == l) 
	return (Item*)this;
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
    if ( breakable_b_ )
	Score_elem::handle_prebroken_dependencies();
}
