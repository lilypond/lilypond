#include "scoreline.hh"
#include "staffline.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "symbol.hh"
#include "paper.hh"
#include "pcol.hh"
#include "pscore.hh"


String
Line_of_score::TeXstring() const
{
     String s("\\vbox{%<- line of score\n");
     for (iter_top(staffs,sc); sc.ok(); sc++){
	 s += sc->TeXstring();
	 if ((sc+1).ok())
	     s+= "\\interstaffline\n";
     }
     s += "}";
     return s;
}


Line_of_score::Line_of_score(svec<PCol *> sv,
			     PScore *ps)
{
    pscore_ = ps;
    for (int i=0; i< sv.sz(); i++) {
	PCol *p=(PCol *) sv[i];
	cols.bottom().add(p);
	p->line=this;
    }

    for (iter_top(pscore_->staffs,sc); sc.ok(); sc++)
	staffs.bottom().add(new Line_of_staff(this, sc));    
}
/* construct a line with the named columns. Make the line field
    in each column point to this
    
    #sv# isn't really const!!
    */


void
Line_of_score::process()
{
    for (iter_top(staffs,i); i.ok(); i++)
	i->process();
}
	
