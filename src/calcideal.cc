#include "idealspacing.hh"
#include "score.hh"
#include "pscore.hh"
#include "paperdef.hh"
#include "scorecolumn.hh"
#include "dimen.hh"


/**
  this needs A LOT of rethinking.

  generate springs between columns.

  */
void
Score::calc_idealspacing()
{
    iter_top(cols_,i);

    for (; i.ok(); i++) {
	assert(i->used_b());
	PCursor<Score_column*> j(i+1);
	if (i->musical_b()) {
	    assert(j.ok());
	    for (int n=0; n < i->durations.size(); n++) {
		Moment d = i->durations[n];
		Real dist = paper_p_->duration_to_dist(d);
		Real strength =  i->durations[0]/i->durations[n];
		assert(strength <= 1.0);
		
		while (j->when() < d + i->when())
		    j++;
		Moment delta_desired = j->when() - (d+i->when());
		dist += paper_p_->duration_to_dist(delta_desired);
		
		pscore_p_->connect(i->pcol_l_, j->pcol_l_, dist, strength);
	    }
	} else if (j.ok()) {
	    
	    /* attach i to the next column in use. This exists, since
	      the last col is breakable, and therefore in use
	      */
	    
	    Moment d = j->when() - i->when();
	    Real dist = (d) ? paper_p_->duration_to_dist(d) : 2 PT; // todo
	    
	    pscore_p_->connect(i->pcol_l_, j->pcol_l_, dist, (d) ? 1.0:1.0);
	}
	// !j.ok() might hold if we're at the last col.
    }
}


