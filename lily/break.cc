/*
  break.cc -- implement Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "break.hh"
#include "paper-def.hh"
#include "line-spacer.hh"
#include "debug.hh"
#include "scoreline.hh"
#include "p-score.hh"
#include "p-col.hh"

///  return all breakable columns
Line_of_cols
Break_algorithm::find_breaks() const
{
    Line_of_cols retval;
    for (iter_top(pscore_l_->col_p_list_,c); c.ok(); c++) {
	if (c->breakable_b())
	    retval.push(c);
    }
    if ( linelength <=0)
	while ( retval.size() >2)
	    retval.del(1);

    return retval;
}
 
Line_spacer*
Break_algorithm::generate_spacing_problem(Line_of_cols curline)const
{
    Line_spacer * sp= (*get_line_spacer)();
    sp->paper_l_ = pscore_l_->paper_l_;
    sp->add_column(curline[0], true, 0.0);
    for (int i=1; i< curline.size()-1; i++)
	sp->add_column(curline[i]);

    if ( linelength > 0)
	sp->add_column(curline.top(), true, linelength);
    else
	sp->add_column(curline.top());
    return sp;
}

Col_hpositions
Break_algorithm::stupid_solution(Line_of_cols curline)const
{
    Line_spacer *sp =generate_spacing_problem(curline);
    Col_hpositions colhpos;
    colhpos.cols = curline;
    colhpos.energy = INFTY;
    colhpos.ugh_b_ = true;
    colhpos.config = sp->default_solution();
    delete sp;
    return colhpos;
}

/// construct an appropriate Spacing_problem and solve it. 
Col_hpositions
Break_algorithm::solve_line(Line_of_cols curline) const
{
    Line_spacer *sp = generate_spacing_problem(curline);
    sp->prepare();
   
    Array<Real> the_sol=sp->solve();
    Col_hpositions col_hpos;
    col_hpos.cols = curline;
    col_hpos.energy = the_sol.pop();
    col_hpos.config = the_sol;
    col_hpos.error_col_l_arr_ = sp->error_pcol_l_arr();
    col_hpos.OK();
    delete sp;
   
    return col_hpos;
}

Break_algorithm::Break_algorithm(PScore&s)
{
    pscore_l_ = &s;
    get_line_spacer =0;
    linelength = s.paper_l_->linewidth_f();
}

bool
Break_algorithm::feasible(Line_of_cols curline) const
{
    if (linelength <=  0)
	return true;
    
    Real l =0;
    for (int i=0; i < curline.size(); i++)
	l +=curline[i]->width().length();
    return l < linelength;    
}

void
Break_algorithm::problem_OK() const
{
    if (!pscore_l_->col_p_list_.size())
	error("Score does not have any columns");
    OK();
}

void
Break_algorithm::OK()const
{
#ifndef NDEBUG
    iter_top(pscore_l_->col_p_list_,start);
    PCursor<PCol *> end (pscore_l_->col_p_list_.bottom());
    
    assert(start->breakable_b());    
    assert(end->breakable_b());
#endif
}

Array<Col_hpositions>
Break_algorithm::solve()const
{
    return do_solve();
}

