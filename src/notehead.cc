#include "misc.hh"

#include "notehead.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper.hh"
#include "lookup.hh"
#include "molecule.hh"


Notehead::Notehead(int ss)
{
    x_dir = 0;
    staff_size=ss;
    position = 0;
    balltype = 0;
    dots = 0;
}

void
Notehead::print()const
{
#ifndef NPRINT
    mtor << "Head "<< balltype << ", position = "<< position
	 << "dots " << dots;
    Item::print();
#endif
}

void
Notehead::preprocess()
{
    brew_molecole();
}

int
Notehead::compare(Notehead*&a, Notehead*&b)
{
    return a->position - b->position;
}

void
Notehead::brew_molecole()
{
    assert(pstaff_);
    assert(!output);

    Paperdef *p = paper();

    Real dy = p->internote();
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
    output->translate(Offset(x_dir * p->note_width(),0));
    bool streepjes = (position<-1)||(position > staff_size+1);
    if (streepjes) {
	int dir = sign(position);
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

