#include "request.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "clef.hh"
#include "staff.hh"
#include "command.hh"
#include "complexstaff.hh"
#include "sccol.hh" 
#include "complexwalker.hh"
#include "score.hh"
#include "pscore.hh"
#include "staffsym.hh"


Complex_column::Complex_column(Score_column*s, Complex_staff *staff_l)
    : Staff_column(s)
{
    staff_l_ = staff_l;
}

Complex_staff::Complex_staff()
{
    pstaff_l_ = 0;
}

void
Complex_column::setup_requests()
{
    for (int i = 0 ; i < v_elts.size(); i ++)
	for (iter_top(v_elts[i]->reqs,j); j.ok(); j++) {	    

	    if (j->barcheck()) {
		if (tdescription_->whole_in_measure) {
		    warning( "Barcheck failed", j->defined_ch_c_l_m );
//		    staff_l_->the_line_->pscore_l_->heu errorlevel_i_ |= 1;
		}
		continue;
	    }
	    if (j->mark())
		continue;
	    if (j->command())
		continue;
	    if (j->groupchange()) // ugh
		first_l_arr_.push(j);
	    else
		second_l_arr_.push(j);
	}
}

Staff_column*
Complex_staff::create_col(Score_column*s)
{
    return new Complex_column(s,this);
}

void
Complex_staff::walk()
{
    for (Complex_walker sc(this); sc.ok(); sc++) {
	sc.col()->setup_requests();
	sc.process();
    }
    Staff_symbol *span_p = new Staff_symbol(5);

	
    Score_column* col_last
	=score_l_->find_col(score_l_->last(), false);
    Score_column* col_first=
	score_l_->find_col(0, false);
    col_first->pcol_l_->set_breakable();
    col_last->pcol_l_->set_breakable();
	
    span_p->set_extent( col_first->pcol_l_->postbreak_p_,
		       col_last->pcol_l_->prebreak_p_);

    pscore_l_->typeset_spanner(span_p, pstaff_l_);
}
