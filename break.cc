/*
    do calculations for breaking problem
    
    */

#include "linespace.hh"
#include "debug.hh"
#include "line.hh"
#include "pscore.hh"

// construct an appropriate Spacing_problem and solve it. 
svec<Real>
PScore::solve_line(svec<const PCol *> curline) const
{
   Spacing_problem sp;
   mtor << "line of " << curline.sz() << " cols\n";
   sp.add_column(curline[0], true, 0.0);
   for (int i=1; i< curline.sz()-1; i++)
       sp.add_column(curline[i]);
   sp.add_column(curline.last(), true, linewidth);

   // misschien  moeven uit Spacing_problem? 
   for (PCursor<Idealspacing *> i(suz); i.ok(); i++) {
       sp.add_ideal(i);
   }
   svec<Real> the_sol=sp.solve();
   return the_sol;
}


void
PScore::problem_OK() 
{
    if (!cols.size())
	error("PScore::problem_OK(): Score does not have any columns");
    PCursor<PCol *> start(cols);
    PCursor<PCol *> end (cols.bottom());
    
    assert(start->breakable);    
    assert(end->breakable);
}

struct Col_configuration {
    svec<const PCol*> line;
    svec<Real> config;
    Real energy;

    Col_configuration() {
	energy = INFTY;
    }
    void add(const PCol*c) { line.add(c);}
    void setsol(svec<Real> sol) {
	config = sol;
	energy = config.last();
	config.pop();
    }
};

/// wordwrap type algorithm
/* el stupido. This should be optimised:

   It would be nice to have a Dynamic Programming type of algorithm
   similar to TeX's
   
    */

void
PScore::calc_breaking()
{
    problem_OK();
    PCursor<PCol *> curcol(cols);
	    
    svec<const PCol *> breakpoints(find_breaks());
    assert(breakpoints.sz()>=2);
    for (int i=0 ; i < breakpoints.sz() -1; ) {
	Col_configuration minimum;
	Col_configuration current;

        // do  another line 
	current.add(breakpoints[i]->postbreak );
	curcol++;		// skip the breakable.
	i++;

	while (i < breakpoints.sz()) {

	    // add another measure.
	    while(breakpoints[i] !=curcol){
		
		current.add(curcol);
		curcol++;
	    }
	    current.add(breakpoints[i]->prebreak );
	    current.setsol(solve_line(current.line));
	    mtor << "energy : " << current.energy << '\n';
	    
	    if (current.energy < minimum.energy) {
		minimum = current;
	    } else {
		break;
	    }
	
	    current.line.last()=breakpoints[i];
	    curcol ++;
	    i++;
	}
	mtor << "Adding line, next breakpoint " << i << '\n';
	add_line(minimum.line, minimum.config);	
    }
}

