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

String
Col_stats::str() const { 
    String s(count_i_);
    s += " lines";
    if  (count_i_)
	s += String(Real(cols_i_)/count_i_, ", (with an average of %.1f columns)");
    
    return s;
}

void
Col_stats::add(Line_of_cols const& line)
{
    count_i_++;
    cols_i_ += line.size();
}


Col_stats::Col_stats()
{
    count_i_ =0;
    cols_i_ =0;
}

/* **************************************************************** */

Line_of_cols
Break_algorithm::all_cols()const
{
    Line_of_cols retval;
    for (PCursor<PCol*> c(pscore_l_->col_p_list_.top()); 
	 c.ok(); c++) {
	
	retval.push(c);
    }
    return retval;
}

Array<int> 
Break_algorithm::find_break_indices() const
{
    Line_of_cols all(all_cols());
    Array<int> retval;
    
    for (int i=0; i < all.size(); i++)
	if (all[i]->breakable_b())
	    retval.push(i);
    
    if ( linelength <=0)
	while ( retval.size() >2)
	    retval.del(1);

    return retval;
}

///  return all breakable columns
Line_of_cols
Break_algorithm::find_breaks() const
{
    Line_of_cols all(all_cols());
    Line_of_cols retval;
    
    for (int i=0; i < all.size(); i++)
	if (all[i]->breakable_b())
	    retval.push(all[i]);


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

    sp->prepare();
    return sp;
}

Break_algorithm::Break_algorithm()
{
    pscore_l_ = 0;
    get_line_spacer =0;
    linelength = 0;
}

void
Break_algorithm::set_pscore(PScore*s)
{
    pscore_l_ = s;
    linelength = s->paper_l_->linewidth_f();
    do_set_pscore();
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

void
Break_algorithm::do_set_pscore()
{
    
}

void
Break_algorithm::print_stats()const
{
    if (approx_stats_.count_i_)
	*mlog << "\nApproximated: " << approx_stats_.str() << "\n";
    if (exact_stats_.count_i_)
	*mlog << "Calculated exactly: " << exact_stats_.str() << "\n";
}
