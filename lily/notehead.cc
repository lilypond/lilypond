/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "misc.hh"
#include "note-head.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "musical-request.hh"

/*
  TODO
  
  Separate notehead into 


     Rhythmic_head
       Note_head
       Rest

     and Stem takes Rhythmic_heads 
 */


Note_head::Note_head(int ss)
{
    x_dir_i_ = 0;
    staff_size_i_=ss;
    position_i_ = 0;
    balltype_i_ = 0;
    dots_i_ = 0;
    dot_delta_y_i_ = 0;
    extremal_i_ = 0;
    rest_b_ = false;
}

void
Note_head::do_pre_processing()
{
    // 8 ball looks the same as 4 ball:
    if (balltype_i_ > 4 && !rest_b_)
	balltype_i_ = 4;
    	
    if (rest_b_) { 
	if (balltype_i_ == 1)
	    position_i_ += 6;
	else if (balltype_i_ == 2)
	    position_i_ += 4;
    }
}

void
Note_head::set_rhythmic(Rhythmic_req*r_req_l)
{
    balltype_i_ = r_req_l->duration_.type_i_;
    dots_i_ = r_req_l->duration_.dots_i_;
}
    

IMPLEMENT_IS_TYPE_B1(Note_head,Item);

void
Note_head::do_print()const
{
#ifndef NPRINT
    if (rest_b_)
	mtor << "REST! ";
    mtor << "balltype_i_ "<< balltype_i_ << ", position_i_ = "<< position_i_
	 << "dots_i_ " << dots_i_;
#endif
}


int
Note_head::compare(Note_head *const  &a, Note_head * const &b)
{
    return a->position_i_ - b->position_i_;
}

void
Note_head::set_dots()
{
    if (!(position_i_ %2) && rest_b_ && balltype_i_ == 1)
	dot_delta_y_i_ = -1;
    else if (!(position_i_ %2))
	dot_delta_y_i_ = 1;
}

/*
  Ugh, hairy.
 */
Molecule*
Note_head::brew_molecule_p() const 
{
    ((Note_head*)this)->set_dots(); // UGH GUH
    Molecule*out = 0;
    Paper_def *p = paper();
    Real inter_f = p->internote_f();
    Symbol s;

    // ugh
    bool streepjes_b = (position_i_<-1) || (position_i_ > staff_size_i_+1);
    
    if (!rest_b_)
	s = p->lookup_l()->ball(balltype_i_);
    else {
	s = p->lookup_l()->rest(balltype_i_, streepjes_b);
    }
    out = new Molecule(Atom(s));
    out->translate_x( x_dir_i_ * s.dim.x.length() );
    if (dots_i_) {
	Symbol d = p->lookup_l()->dots(dots_i_ );
	Molecule dm;
	dm.add(Atom(d));
	dm.translate_y( inter_f * dot_delta_y_i_ );
	out->add_right(dm);
    }

    
    if (rest_b_) {
	streepjes_b = false;
    }
    
    if (streepjes_b) {
	int dir = sign(position_i_);
	int s =(position_i_<-1) ? -((-position_i_)/2): (position_i_-staff_size_i_)/2;
	
	Symbol str = p->lookup_l()->streepjes(s);
	Molecule sm;
	sm.add(Atom(str));
	if (position_i_ % 2)
	    sm.translate_y(-inter_f* dir);
	out->add(sm);	    
    }
    
    out->translate_y(inter_f*position_i_);
    return out;
}

