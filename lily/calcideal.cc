#include "idealspacing.hh"
#include "score.hh"
#include "p-score.hh"
#include "paper-def.hh"
#include "score-column.hh"
#include "dimen.hh"


/**
  generate springs between columns.

  TODO: This needs A LOT of rethinking.  Spacing should take optical
  effects into account, should be local (measure wide), should check
  smallest divisions.
    

  */
void
Score::calc_idealspacing()
{
    iter_top(cols_,i);

    for (; i.ok(); i++) {
	if (!i->used_b())
	    continue;
	
	PCursor<Score_column*> j(i+1);

	if (i->musical_b()) {
	    assert(j.ok());
	    for (int n=0; n < i->durations.size(); n++) {
		Moment d = i->durations[n];
		Real dist = paper_p_->duration_to_dist(d);
		Real strength =  i->durations[0]/i->durations[n];
		assert(strength <= 1.0);
		
		while (j.ok()) {
		    if (j->used_b() && j->when() >= d + i->when() )
			break;
		    j++;
		}
		Moment delta_desired = j->when() - (d+i->when());
		dist += paper_p_->duration_to_dist(delta_desired);
		
		pscore_p_->connect(i->pcol_l_, j->pcol_l_, dist, strength);
	    }
	} else if (j.ok()) {
	    while  (!j->used_b())
		j++;
	    
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


