#include "request.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "staff.hh"
#include "lyricstaff.hh"
#include "lyriccolumn.hh"
#include "sccol.hh" 
#include "pscore.hh"
#include "main.hh"

Lyric_column::Lyric_column(Score_column* s, Lyric_staff* lstaff_l)
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
		    warning( "Barcheck failed ", req_l->defined_ch_c_l_m );
		}
	    }
	    if (req_l->lreq_l()) {
		winfo_array_.push(req_l->lreq_l());
	    }
	}
    }
}

void
Lyric_column::typeset_item(Item *i)
{    
    lstaff_l_->pscore_l_->typeset_item(i, score_column_l_->pcol_l_,
				  lstaff_l_->pstaff_l_);
}    

Word_info::Word_info()
{
    lreq_l_ = 0;
}

Word_info::Word_info(Lyric_req* lreq_l) 
{
    lreq_l_ = lreq_l;
}
