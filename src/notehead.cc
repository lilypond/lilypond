#include "misc.hh"
#include "notehead.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "request.hh"

NAME_METHOD(Notehead);

Notehead::Notehead(int ss)
{
    x_dir = 0;
    staff_size=ss;
    position = 0;
    balltype = 0;
    dots = 0;
    extremal = 0;
    defined_ch_c_l_m = 0;
}

void
Notehead::set_rhythmic(Rhythmic_req*r_req_l)
{
    balltype = r_req_l->balltype;
    dots = r_req_l->dots;
    if ( r_req_l->defined_ch_c_l_m )
	defined_ch_c_l_m = r_req_l->defined_ch_c_l_m;
}
    
void
Notehead::do_print()const
{
#ifndef NPRINT
    mtor << "balltype "<< balltype << ", position = "<< position
	 << "dots " << dots;
#endif
}


int
Notehead::compare(Notehead*&a, Notehead*&b)
{
    return a->position - b->position;
}

Molecule*
Notehead::brew_molecule_p() const return out;
{
    Paperdef *p = paper();

    Real dy = p->internote();
    Symbol s = p->lookup_p_->ball(balltype);
    
    out = new Molecule(Atom(s));
    if (dots) {
	Symbol d = p->lookup_p_->dots(dots);
	Molecule dm;
	dm.add(Atom(d));
	if (!(position %2))
	    dm.translate(Offset(0,dy));
	out->add_right(dm);
    }
    out->translate(Offset(x_dir * p->note_width(),0));
    bool streepjes = (position<-1)||(position > staff_size+1);
    if (streepjes) {
	int dir = sign(position);
	int s =(position<-1) ? -((-position)/2): (position-staff_size)/2;
	Symbol str = p->lookup_p_->streepjes(s);
	Molecule sm;
	sm.add(Atom(str));
	if (position % 2)
	    sm.translate(Offset(0,-dy* dir));
	out->add(sm);	    
    }
    
    out->translate(Offset(0,dy*position));
}

