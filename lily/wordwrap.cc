/*
  wordwrap.cc -- implement Word_wrap

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "break.hh"
#include "p-score.hh"
#include "debug.hh"
#include "p-col.hh"
/** el stupido. This should be done more accurately:

   It would be nice to have a Dynamic Programming type of algorithm
   similar to TeX's
   
    */
Array<Col_hpositions>
Word_wrap::solve()
{
    problem_OK();
    iter_top(pscore_.cols,curcol);
    Array<Col_hpositions> breaking;
    Line_of_cols breakpoints(find_breaks());
    assert(breakpoints.size()>=2);

    int break_idx_i=0;			
    while ( break_idx_i < breakpoints.size() -1) {
	Col_hpositions minimum;
	Col_hpositions current;

        // do  another line
	PCol *post = breakpoints[break_idx_i]->postbreak_p_;
	current.add( post);
	curcol++;		// skip the breakable.
	break_idx_i++;

	while (break_idx_i < breakpoints.size()) {

	    // add another measure.
	    while (breakpoints[break_idx_i] != curcol.ptr()){
		current.add(curcol);
		curcol++;
	    }
	    current.add(breakpoints[break_idx_i]->prebreak_p_ );

	    // try to solve
	    if (!feasible(current.cols)) {
		if (!minimum.cols.size()) {
		    warning("Ugh, this measure is too long, breakpoint: "
			  + String(break_idx_i) +
			" (generating stupido solution)");
		    current = stupid_solution(current.cols);
		    current.energy = - 1; // make sure we break out.
		} else
		    current.energy = INFTY;	// make sure we go back
	    } else {
		current = solve_line(current.cols);
		current.print();
	    }

	    // update minimum, or backup.
	    if (current.energy < minimum.energy || current.energy < 0) {		
		minimum = current;	   
	    } else {		// we're one col too far.
		break_idx_i--;
		while (curcol.ptr() != breakpoints[break_idx_i])
		    curcol --;
		break;		// do the next line.
	    }


	    // add nobreak version of breakable column
	    current.cols.top()=breakpoints[break_idx_i];
	    curcol ++;
	    break_idx_i++;
	}

	*mlog << "[" <<break_idx_i<<"]"<<flush;
	breaking.push(minimum);
    }
    
    return breaking;
}

Word_wrap::Word_wrap(PScore&ps)
    : Break_algorithm(ps)
{
}
