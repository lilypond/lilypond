#include "request.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "staff.hh"
//#include "command.hh"
#include "lyricstaff.hh"
#include "lyriccolumn.hh"
#include "sccol.hh" 
#include "pscore.hh"
//#include "paper.hh"


Lyric_column::Lyric_column(Score_column*s, Lyric_staff* lstaff_l)
    : Staff_column(s)
{
    lstaff_l_ = lstaff_l;
}

void
Lyric_column::setup_requests()
{
    for (int i = 0 ; i < v_elts.size(); i ++) {
	for (iter_top(v_elts[i]->reqs,j); j.ok(); j++) {
	    Request* req_l = j;
	    if (req_l->barcheck()) {
		if (tdescription_->whole_in_measure) {
		    error("Barcheck failed, " + tdescription_->str());
		}
	    }
	    if (req_l->lreq_l()) {
		winfo_array_.push(req_l->lreq_l());
	    }
	}
    }
}

Interval itemlist_width(const Array<Item*> &its);

void
Lyric_column::typeset_item(Item *i, int breakst)
{
    assert(i);
    
    lstaff_l_->pscore_l_->typeset_item(i, score_column_l_->pcol_l_,
				  lstaff_l_->line_pstaff_p_,breakst);
    
    if (breakst == BREAK_PRE - BREAK_PRE) {
	
        Array<Item*> to_move(
	    lstaff_l_->pscore_l_->select_items(lstaff_l_->line_pstaff_p_,
					  score_column_l_->pcol_l_->prebreak_p_));
	Interval column_wid = itemlist_width(to_move);
	assert(!column_wid.empty());

	for (int j=0; j < to_move.size(); j++) {
	    to_move[j]->translate(Offset(-column_wid.right, 0));
	}
    }
}    

Word_info::Word_info()
{
    lreq_l_ = 0;
}

Word_info::Word_info(Lyric_req* lreq_l) 
{
    lreq_l_ = lreq_l;
}
