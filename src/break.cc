/*
    do calculations for breaking problem
    
    */
#include "break.hh"
#include "paper.hh"
#include "linespace.hh"
#include "debug.hh"
#include "scoreline.hh"
#include "pscore.hh"


/*
  return all breakable columns
 */
Array<PCol *>
Break_algorithm::find_breaks() const
{
    Array<PCol *> retval;
    for (iter_top(pscore_.cols,c); c.ok(); c++)
	if (c->breakable())
	    retval.push(c);

    return retval;
}

// construct an appropriate Spacing_problem and solve it. 
Array<Real>
Break_algorithm::solve_line(Line_of_cols curline) const
{
   Spacing_problem sp;

   sp.add_column(curline[0], true, 0.0);
   for (int i=1; i< curline.size()-1; i++)
       sp.add_column(curline[i]);
   sp.add_column(curline.last(), true, linelength);

   // misschien  moeven uit Spacing_problem? 
   for (iter_top(pscore_.suz,i); i.ok(); i++) {
       sp.add_ideal(i);
   }
   Array<Real> the_sol=sp.solve();
   return the_sol;
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
    
    assert(start->breakable());    
    assert(end->breakable());
#endif
}

/****************/

Col_configuration::Col_configuration()
{
    energy = INFTY;
}

void
Col_configuration::add( PCol*c)
{
    cols.push(c);
}

void
Col_configuration::setsol(Array<Real> sol)
{
    config = sol;
    energy = config.last();
    config.pop();
}

void
Col_configuration::print() const
{
#ifndef NPRINT
    mtor << "energy : " << energy << '\n';
    mtor << "line of " << config.size() << " cols\n";
#endif
}
void
Col_configuration::OK()const
{
#ifndef NDEBUG
    assert(config.size() == cols.size());
#endif
}
