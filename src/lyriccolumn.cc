#include "musicalrequest.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "staff.hh"
#include "lyricstaff.hh"
#include "lyriccolumn.hh"
#include "sccol.hh" 
#include "pscore.hh"
#include "main.hh"

Lyric_column::Lyric_column(Lyric_staff* lstaff_l)
{
    lstaff_l_ = lstaff_l;
}

void
Lyric_column::setup_one_request(Request*req_l)
{
    if (req_l->lreq_l()) 
	lreq_l_array_.push(req_l->lreq_l());   
}

void
Lyric_column::typeset_item(Item *i)
{    
    lstaff_l_->pscore_l_->typeset_item(i, musical_column_l_->pcol_l_,
				  lstaff_l_->pstaff_l_);
}
