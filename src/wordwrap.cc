#include "break.hh"
#include "pscore.hh"
#include "debug.hh"

/* el stupido. This should be done more accurately:

   It would be nice to have a Dynamic Programming type of algorithm
   similar to TeX's
   
    */

Array<Col_configuration>
Word_wrap::solve()
{
    problem_OK();
    iter_top(pscore_.cols,curcol);
    Array<Col_configuration> breaking;
    Array<PCol *> breakpoints(find_breaks());
    assert(breakpoints.size()>=2);
    for (int i=0 ; i < breakpoints.size() -1; ) {
	Col_configuration minimum;
	Col_configuration current;

        // do  another line
	PCol *post = breakpoints[i]->postbreak_p_;
	current.add( post);
	curcol++;		// skip the breakable.
	i++;

	while (i < breakpoints.size()) {

	    // add another measure.
	    while (breakpoints[i] != curcol.ptr()){
		
		current.add(curcol);
		curcol++;
	    }
	    current.add(breakpoints[i]->prebreak_p_ );
	    if (!feasible(current.cols)) {
		if (!minimum.cols.size())
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
	breaking.push(minimum);
    }
    
    return breaking;
}

Word_wrap::Word_wrap(PScore&ps)
    : Break_algorithm(ps)
{
}
