#include "tstream.hh"
#include "score.hh"
#include "pscore.hh"
#include "staff.hh"
#include "paper.hh"
#include "sccol.hh"
#include "debug.hh"
#include "dimen.hh"


void
Score::do_connect(PCol *c1, PCol *c2, Real d, Real h)
{
    if (!c1 || !c2 )
	return;
    Idealspacing*sp=pscore_->get_spacing(c1,c2);
	
    if (!sp->hooke){
	sp->hooke = h;
	sp->space =d;
    }
}

void
Score::connect(PCol* c1, PCol *c2, Real d, Real h)
{
    if (c2->used() && c1->used()) {
	do_connect(c1,c2,d,h);
	do_connect(c1->postbreak, c2,d,h);
	do_connect(c1, c2->prebreak,d,h);
	do_connect(c1->postbreak, c2->prebreak,d,h);
    }
}

/* this needs A LOT of rethinking.

    generate springs between columns.
    */
void
Score::calc_idealspacing()
{
    PCursor<Score_column*> i(cols_);

    for (; i.ok(); i++) {
		
	PCursor<Score_column*> j (i+1);
	if (i->musical) {
	    for (int n=0; n < i->durations.sz(); n++) {
		Real d = i->durations[n];
		Real dist = paper_->duration_to_dist(d);
		while (d + i->when > j->when)
		    j++;

		if (j->used())
		    connect(i->pcol_, j->pcol_, dist);
		if (!j->musical && (j+1)->used && (j+1)->when == j->when) {
		    j++;
		    connect(i->pcol_, j->pcol_,  dist);
		}
	    }
	} else if (i->used()) {

	    /* attach i to the next column in use. This exists, since
	      the last col is breakable, and therefore in use
	      */
	    for (;j.ok(); j++) {
		if (j->used()) {
		    Real d = j->when - i->when;
		    Real dist = (d) ? paper_->duration_to_dist(d) :
			convert_dimen(2,"pt");

		    connect(i->pcol_, j->pcol_, dist, (d) ? 1.0:1.0);
		    break;
		}
	    }
	    // !j.ok() might hold if we're at the last col.
	}
    }       	
}


