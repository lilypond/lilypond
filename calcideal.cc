#include "tstream.hh"
#include "score.hh"
#include "pscore.hh"
#include "staff.hh"
#include "paper.hh"
#include "misc.hh"
#include "sccol.hh"
#include "debug.hh"


void
Score::do_connect(PCol *c1, PCol *c2, Real d)
{
    Idealspacing*sp=pscore_->get_spacing(c1,c2);
	
    if (!sp->hooke){
	sp->hooke = 1.0;
	sp->space =d;
    }
}

void
Score::connect_nonmus(PCol* c1, PCol *c2, Real d)
{
    if (c2->used && c1->used) {
	do_connect(c1,c2,d);

	// alert! this is broken!
	if (c1->breakable()) {
	    do_connect(c1->postbreak, c2,d);
	}
	if (c2->breakable()) {
	    do_connect(c1, c2->prebreak,d);
	}
	if (c1->breakable() &&c2->breakable()) {
	    do_connect(c1->postbreak, c2->prebreak,d);	    
	}	
    }
}
/* this needs A LOT of rethinking.

    generate springs between columns.
    */
void
Score::calc_idealspacing()
{
    PCursor<Score_column*> sc(cols_);

    for (; sc.ok(); sc++) {
	if (sc->musical)
	    for (int i=0; i < sc->durations.sz(); i++) {
		Real d = sc->durations[i];
		Real dist = duration_to_idealspace(d, paper_->whole_width);
		PCol * c2 = find_col(sc->when + d,true)->pcol;
		connect_nonmus(sc->pcol, c2, dist);
		c2 = find_col(sc->when + d,false)->pcol;
		connect_nonmus(sc->pcol, c2,  dist);
	    }
	else if (sc->used()) {	// ignore empty columns
	    PCol * c2 = find_col(sc->when,true)->pcol;
	    connect_nonmus(sc->pcol,c2,0.0);
	}
    }       	
}


