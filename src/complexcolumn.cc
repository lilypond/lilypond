/*
  complexcolumn.cc -- implement Complex_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "voice.hh"
#include "debug.hh"
#include "complexcolumn.hh"
#include "pscore.hh"
#include "request.hh"
#include "pscore.hh"
#include "sccol.hh"
#include "complexstaff.hh"
#include "misc.hh"

void
Complex_column::typeset_musical_item(Item *i)
{
    assert(i);
    Score_column * sccol_l = musical_column_l_;
    staff_l_->pscore_l_->typeset_item(i, sccol_l->pcol_l_,
				      staff_l_->pstaff_l_);
}

Complex_column::Complex_column(Complex_staff *staff_l)
{
    staff_l_ = staff_l;
}

void
Complex_column::setup_one_request(Request * j)
{
    if (j->nonmus()) // ugh
	first_l_arr_.push(j);
    else if (j->musical())
	second_l_arr_.push(j);
}

/**
  align items in #item_l_arr#, return the width.
 */
Interval
align_items(Array<Item*> item_l_arr)
{
    Interval wid(0,0);
    for  (int i =0; i < item_l_arr.size(); i++) {
	Interval item_width= item_l_arr[i]->width();
	item_l_arr[i]->translate(Offset( wid.right - item_width.left ,0));
	wid.unite(item_width);
    }
    return wid;
}

void 
translate_items(Real x,  Array<Item*> item_l_arr)
{
    for  (int i =0; i < item_l_arr.size(); i++) 
	item_l_arr[i]->translate(Offset(x, 0));
}
/*
  UGR
  This still sux
  */
void
Complex_column::typeset_breakable_items(Array<Item *> &pre_p_arr,
					Array<Item *> &nobreak_p_arr,
					Array<Item *> &post_p_arr)
{
    PCol * c= command_column_l_->pcol_l_;
    PScore *ps_l=staff_l_->pscore_l_;
    if (!c->breakable_b()) {	  
	for  (int i =0; i < pre_p_arr.size(); i++)
	    delete pre_p_arr[i];
	pre_p_arr.set_size(0);
	for  (int i =0; i < post_p_arr.size(); i++)
	    delete post_p_arr[i];
	post_p_arr.set_size(0);
    }

      
    for  (int i =0; i < pre_p_arr.size(); i++) {
	ps_l->typeset_item(pre_p_arr[i], c, staff_l_->pstaff_l_,0);
    }
    for  (int i =0; i < nobreak_p_arr.size(); i++) {
	ps_l->typeset_item(nobreak_p_arr[i], c, staff_l_->pstaff_l_,1);
    }
    for  (int i =0; i < post_p_arr.size(); i++) {
	ps_l->typeset_item(post_p_arr[i], c, staff_l_->pstaff_l_,2);
    }

    Interval pre_wid= align_items(pre_p_arr);
    translate_items( -pre_wid.right, pre_p_arr);
    align_items(nobreak_p_arr);
    Interval post_wid =align_items(post_p_arr);
    translate_items (-post_wid.left , post_p_arr);

    pre_p_arr.set_size(0);
	post_p_arr.set_size(0);
   nobreak_p_arr.set_size(0);
}
