#include "notehead.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "pstaff.hh"
#include "pscore.hh"
#include "paper.hh"
#include "lookupsyms.hh"
#include "molecule.hh"


Notehead::Notehead(int ss)
{
    staff_size=ss;
    position = 0;
    balltype = 0;
    dots = 0;
}

void
Notehead::print()const
{
    mtor << "Head "<<balltype<<", position = "<< position << "dots " << dots;
    Item::print();
}

void
Notehead::preprocess()
{
    brew_molecole();
}

void
Notehead::brew_molecole()
{
    assert(pstaff_);
    assert(!output);

    Paperdef *p = pstaff_->pscore_->paper_;

    Real dy = p->interline()/2;
    Symbol s = p->lookup_->ball(balltype);
    
    output = new Molecule(Atom(s));
    if (dots) {
	Symbol d = p->lookup_->dots(dots);
	Molecule dm;
	dm.add(Atom(d));
	if (!(position %2))
	    dm.translate(Offset(0,dy));
	output->add_right(dm);
    }
    bool streepjes = (position<-1)||(position > staff_size+1);
    if (streepjes) {
	int dir = sgn(position);
	int s =(position<-1) ? -((-position)/2): (position-staff_size)/2;
	Symbol str = p->lookup_->streepjes(s);
	Molecule sm;
	sm.add(Atom(str));
	if (position % 2)
	    sm.translate(Offset(0,-dy* dir));
	output->add(sm);	    
    }
    

    output->translate(Offset(0,dy*position));
}

