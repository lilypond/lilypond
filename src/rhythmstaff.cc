#include "molecule.hh"
#include "score.hh"
#include "request.hh"
#include "notehead.hh"
#include "stem.hh"
#include "linepstaff.hh"
#include "rhythmstaff.hh"
#include "paper.hh"
#include "sccol.hh" 
#include "rest.hh"

void
Rhythmic_staff::set_output(PScore*ps)
{
    theline_l_ = new Linestaff(1,ps);
    Simple_staff::set_output(ps);
}

Item *
Rhythmic_staff::get_TYPESET_item(Command *com)
{
    if (com->args[0] == "KEY" || com->args[0] == "CLEF"||
	com->args[0] == "CURRENTCLEF")
	return 0;
    Item *i = Simple_staff::get_TYPESET_item(com);
    if (!i) return 0;
    i->translate(Offset(0,
			-score_l_->paper_p_->standard_height()/2));
    return i;
}

Notehead*
Rhythmic_staff::get_notehead(Note_req *rq, int)
{
    int b = rq->rhythmic()->balltype;
    int d = rq->rhythmic()->dots;

    Notehead *n =new Notehead(1);
    n->balltype = b;
    n->dots =d;
    n->position = 0;
    return n;
}

Stem *
Rhythmic_staff::get_stem(Stem_req*rq, Moment l)
{
    Stem * s = new Stem(0,l);
    s->flag = rq->stem_number;
    return s;    
}

/*
  creation
  */
Staff *
get_new_rhythmstaff()
{
    return new Rhythmic_staff;
}

