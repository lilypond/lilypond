#include "idealspacing.hh"
#include "tstream.hh"
#include "score.hh"
#include "pscore.hh"
#include "staff.hh"
#include "paper.hh"
#include "sccol.hh"
#include "debug.hh"
#include "dimen.hh"


/*
  this needs A LOT of rethinking.

  generate springs between columns.

  */
void
Score::calc_idealspacing()
{
    iter_top(cols_,i);

    for (; i.ok(); i++) {
	assert(i->used());
	PCursor<Score_column*> j(i+1);
	if (i->musical_) {
	    assert(j.ok());
	    for (int n=0; n < i->durations.size(); n++) {
		Moment d = i->durations[n];
		Real dist = paper_p_->duration_to_dist(d);
		while (j->when() < d + i->when())
		    j++;
		assert( j->when()== d+i->when());
		
		pscore_p_->connect(i->pcol_l_, j->pcol_l_, dist);
#if 0		
		if (!j->musical_ && (j+1).ok() 
		    && ) {
		    j++;
		    pscore_p_->connect(i->pcol_l_, j->pcol_l_,  dist);
		}
#endif	
	    }
	} else if (j.ok()) {
	    
	    /* attach i to the next column in use. This exists, since
	      the last col is breakable, and therefore in use
	      */
	    
	    Moment d = j->when() - i->when();
	    Real dist = (d) ? paper_p_->duration_to_dist(d) :
		convert_dimen(2,"pt"); // todo
	    
	    pscore_p_->connect(i->pcol_l_, j->pcol_l_, dist, (d) ? 1.0:1.0);
	}
	// !j.ok() might hold if we're at the last col.
    }
}


