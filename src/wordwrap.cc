#include "break.hh"
#include "pscore.hh"
#include "debug.hh"

/* el stupido. This should be done more accurately:

   It would be nice to have a Dynamic Programming type of algorithm
   similar to TeX's
   
    */

svec<Col_configuration>
Word_wrap::solve()
{
    problem_OK();
    iter_top(pscore_.cols,curcol);
    svec<Col_configuration> breaking;
    svec<PCol *> breakpoints(find_breaks());
    assert(breakpoints.sz()>=2);
    for (int i=0 ; i < breakpoints.sz() -1; ) {
	Col_configuration minimum;
	Col_configuration current;

        // do  another line
	PCol *post = breakpoints[i]->postbreak;
	current.add( post);
	curcol++;		// skip the breakable.
	i++;

	while (i < breakpoints.sz()) {

	    // add another measure.
	    while (breakpoints[i] != curcol.ptr()){
		
		current.add(curcol);
		curcol++;
	    }
	    current.add(breakpoints[i]->prebreak );
	    if (!feasible(current.cols)) {
		if (!minimum.cols.sz())
		    error("sorry, this measure is too long");
		break;
	    }
	    current.setsol(solve_line(current.cols));
	    current.print();
	    
	    if (current.energy < minimum.energy) {		
		minimum = current;	   
	    } else {		// we're one col too far.
		i--;
		while (curcol.ptr() != breakpoints[i])
		    curcol --;
		
		break;
	    }
	
	    current.cols.last()=breakpoints[i];
	    curcol ++;
	    i++;
	}
	mtor << "Adding cols~, next breakpoint " << i << '\n';
	breaking.add(minimum);
    }
    
    return breaking;
}

Word_wrap::Word_wrap(PScore&ps)
    : Break_algorithm(ps)
{
}
