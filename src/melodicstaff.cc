#include "melodicstaff.hh"
#include "stem.hh"
#include "rest.hh"
#include "notehead.hh"
#include "paper.hh"
#include "molecule.hh"
#include "linestaff.hh"
#include "rhythmstaff.hh"
#include "sccol.hh" 

const int NO_LINES=5;
const int BOTTOM_POSITION=2;	// e is on bottom line of 5-staff...

void
Melodic_staff::set_output(PScore*ps)
{
    theline = new Linestaff(NO_LINES,ps);
    Simple_staff::set_output(ps);
}


Notehead*
Melodic_staff::get_notehead(Note_req *rq)
{        
    int b  = rq->rhythmic()->balltype;
    int d  = rq->rhythmic()->dots;
    
    Notehead *n =new Notehead((NO_LINES-1)*2);
    n->balltype =b;
    n->dots = d;
    n->position = rq->note()->height() - BOTTOM_POSITION;
    return n;
}


Stem *
Melodic_staff::get_stem(Stem_req*rq)
{
    Stem * s = new Stem(NO_LINES-1);
    s->flag = rq->stem_number;
    return s;
}

/*
  creation
  */
Staff *
get_new_melodicstaff()
{
    return new Melodic_staff;
}



Melodic_staff*
Melodic_staff::clone()const
{
    return new Melodic_staff(*this);
}
