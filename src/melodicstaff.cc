#include "melodicstaff.hh"
#include "keyitem.hh"
#include "stem.hh"
#include "rest.hh"
#include "notehead.hh"
#include "paper.hh"
#include "molecule.hh"
#include "linestaff.hh"
#include "rhythmstaff.hh"
#include "sccol.hh" 
#include "localkeyitem.hh"
#include "request.hh"

const int NO_LINES=5;


void
Melodic_staff::set_output(PScore*ps)
{
    theline = new Linestaff(NO_LINES,ps);
    Simple_staff::set_output(ps);
}


Notehead*
Melodic_staff::get_notehead(Note_req *rq, int bottom)
{        
    int b  = rq->rhythmic()->balltype;
    int d  = rq->rhythmic()->dots;
    
    Notehead *n =new Notehead((NO_LINES-1)*2);
    n->balltype =b;
    n->dots = d;
    n->position = rq->note()->height() + bottom;
    return n;
}

Item *
Melodic_staff::get_TYPESET_item(Command*com)
{
    if (com->args[0] == "KEY") {
	return new Keyitem(NO_LINES);	// urgh. depends on clef.
    } else
	return Simple_staff::get_TYPESET_item(com);
}

Stem *
Melodic_staff::get_stem(Stem_req*rq, Real dur)
{
    Stem * s = new Stem(NO_LINES-1, dur);
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

Rest*
Melodic_staff::get_rest(Rest_req*rq)
{
    Rest*r = Simple_staff::get_rest(rq);
    if (rq->balltype <= 2)
	r->translate(Offset(0, NO_LINES * paper()->internote()));
    return r;
}

Local_key_item*
Melodic_staff::get_local_key_item()
{
    return new Local_key_item(-2);
}

