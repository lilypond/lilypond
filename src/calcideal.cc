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
    do_connect(c1,c2,d,h);
    do_connect(c1->postbreak, c2,d,h);
    do_connect(c1, c2->prebreak,d,h);
    do_connect(c1->postbreak, c2->prebreak,d,h);
}

/* this needs A LOT of rethinking.

    generate springs between columns.
    */
void
Score::calc_idealspacing()
{
#if 1
    PCursor<Score_column*> i(cols_);

    for (; i.ok(); i++) {
	assert(i->used());
	PCursor<Score_column*> j (i+1);
	if (i->musical) {
	    for (int n=0; n < i->durations.sz(); n++) {
		Real d = i->durations[n];
		Real dist = paper_->duration_to_dist(d);
		while (j->when < d + i->when)
		    j++;
		
		assert(j->when == d+i->when);

		connect(i->pcol_, j->pcol_, dist);
		if (!j->musical && (j+1).ok() 
		    && (j+1)->when == j->when) {
		    j++;
		    connect(i->pcol_, j->pcol_,  dist);
		}
	    }
	} else if (j.ok()) {
	    
	    /* attach i to the next column in use. This exists, since
	      the last col is breakable, and therefore in use
	      */
	    
	    Real d = j->when - i->when;
	    Real dist = (d) ? paper_->duration_to_dist(d) :
		convert_dimen(2,"pt");
	    
	    connect(i->pcol_, j->pcol_, dist, (d) ? 1.0:1.0);
	}
	    // !j.ok() might hold if we're at the last col.
	
    }
#else
    PCursor<Score_column*> sc(cols_);

    for (; sc.ok(); sc++) {
	if (sc->musical)
	    for (int i=0; i < sc->durations.sz(); i++) {
		Real d = sc->durations[i];
		Real dist = paper_->duration_to_dist(d);
		PCol * c2 = find_col(sc->when + d,true)->pcol_;
		connect(sc->pcol_, c2, dist);
		c2 = find_col(sc->when + d,false)->pcol_;
		connect(sc->pcol_, c2,  dist);
	    }
	else if (sc->used()) {	// ignore empty columns
	    PCol * c2 = find_col(sc->when,true)->pcol_;
	    connect(sc->pcol_, c2, 0.0);
	}
#endif
}


