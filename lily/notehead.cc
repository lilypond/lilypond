#include "misc.hh"
#include "notehead.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "musical-request.hh"



Notehead::Notehead(int ss)
{
    x_dir_i_ = 0;
    staff_size_i_=ss;
    position_i_ = 0;
    balltype_i_ = 0;
    dots_i_ = 0;
    extremal_i_ = 0;
    rest_b_ = false;
}

void
Notehead::set_rhythmic(Rhythmic_req*r_req_l)
{
    balltype_i_ = r_req_l->duration_.type_i_;
    dots_i_ = r_req_l->duration_.dots_i_;
}
    
IMPLEMENT_STATIC_NAME(Notehead);

void
Notehead::do_print()const
{
#ifndef NPRINT
    if (rest_b_)
	mtor << "REST! ";
    mtor << "balltype_i_ "<< balltype_i_ << ", position_i_ = "<< position_i_
	 << "dots_i_ " << dots_i_;
#endif
}


int
Notehead::compare(Notehead *const  &a, Notehead * const &b)
{
    return a->position_i_ - b->position_i_;
}

Molecule*
Notehead::brew_molecule_p() const 
{
    Molecule*out = 0;
    Paper_def *p = paper();

    Real dy = p->internote();
    Symbol s;
    if (!rest_b_)
	s = p->lookup_l()->ball(balltype_i_);
    else 
	s = p->lookup_l()->rest(balltype_i_);
    
    out = new Molecule(Atom(s));
    if (dots_i_) {
	Symbol d = p->lookup_l()->dots(dots_i_);
	Molecule dm;
	dm.add(Atom(d));
	if (!(position_i_ %2))
	    dm.translate(Offset(0,dy));
	out->add_right(dm);
    }
    out->translate(Offset(x_dir_i_ * p->note_width(),0));
    bool streepjes = (position_i_<-1)||(position_i_ > staff_size_i_+1);
    
    if (rest_b_ && balltype_i_ > 2)
	streepjes = false;
    
    if (streepjes) {
	int dir = sign(position_i_);
	int s =(position_i_<-1) ? -((-position_i_)/2): (position_i_-staff_size_i_)/2;
	Symbol str = p->lookup_l()->streepjes(s);
	Molecule sm;
	sm.add(Atom(str));
	if (position_i_ % 2)
	    sm.translate(Offset(0,-dy* dir));
	out->add(sm);	    
    }
    
    out->translate(Offset(0,dy*position_i_));
    return out;
}

