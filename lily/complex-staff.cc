#include "complex-staff.hh"
#include "complex-walker.hh"
#include "score.hh"
#include "p-score.hh"
#include "staffsym.hh"
#include "score-column.hh"

const NO_LINES = 5;

/** Aside from putting fields right, this generates the staff symbol.
 */
void
Complex_staff::set_output(PScore* pscore_l )
{
    pstaff_l_ = new PStaff(pscore_l);
    pscore_l_ = pscore_l;
    pscore_l_->add(pstaff_l_);

    Staff_symbol *span_p = new Staff_symbol(NO_LINES);
    
    Score_column* col_last
	=score_l_->find_col(score_l_->last(), false);
    Score_column* col_first=
	score_l_->find_col(0, false);
	
    span_p->set_extent(col_first->pcol_l_->postbreak_p_,
		       col_last->pcol_l_->prebreak_p_);

    pscore_l_->typeset_spanner(span_p, pstaff_l_);
}


Staff_walker * 
Complex_staff::get_walker_p()
{
    return new Complex_walker(this);
}
