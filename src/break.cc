/*
    do calculations for breaking problem    
    */
#include "break.hh"
#include "paper-def.hh"
#include "linespace.hh"
#include "debug.hh"
#include "scoreline.hh"
#include "pscore.hh"


/*
  return all breakable columns
 */
Line_of_cols
Break_algorithm::find_breaks() const
{
    Line_of_cols retval;
    for (iter_top(pscore_.cols,c); c.ok(); c++)
	if (c->breakable_b())
	    retval.push(c);
    assert(retval.top() == pscore_.cols.bottom().ptr());
    return retval;
}

// construct an appropriate Spacing_problem and solve it. 
Col_hpositions
Break_algorithm::solve_line(Line_of_cols curline) const
{
   Spacing_problem sp;

   sp.add_column(curline[0], true, 0.0);
   for (int i=1; i< curline.size()-1; i++)
       sp.add_column(curline[i]);
   sp.add_column(curline.top(), true, linelength);

   // misschien  moeven uit Spacing_problem? 
   for (iter_top(pscore_.suz,i); i.ok(); i++) {
       sp.add_ideal(i);
   }
   Array<Real> the_sol=sp.solve();
   Col_hpositions col_hpos;
   col_hpos.cols = curline;
   col_hpos.energy = the_sol.pop();
   col_hpos.config = the_sol;
   col_hpos.OK();
   return col_hpos;
}

Break_algorithm::Break_algorithm(PScore&s)
    :pscore_(s)
{
    linelength = s.paper_l_->linewidth;
}

bool
Break_algorithm::feasible(Line_of_cols curline) const
{
    Real l =0;
    for (int i=0; i < curline.size(); i++)
	l +=curline[i]->width().length();
    return l < linelength;    
}

void
Break_algorithm::problem_OK() const
{
    if (!pscore_.cols.size())
	error("Score does not have any columns");
#ifndef NDEBUG
    iter_top(pscore_.cols,start);
    PCursor<PCol *> end (pscore_.cols.bottom());
    
    assert(start->breakable_b());    
    assert(end->breakable_b());
#endif
}
