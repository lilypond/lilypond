/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "misc.hh"
#include "dots.hh"
#include "note-head.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "musical-request.hh"


Note_head::Note_head ()
{
  x_dir_ = CENTER;
  staff_size_i_= 8;		// UGH
  position_i_ = 0;
  extremal_i_ = 0;
}

void
Note_head::do_pre_processing ()
{
  // 8 ball looks the same as 4 ball:
  if (balltype_i_ > 2)
    balltype_i_ = 2;
  if (dots_l_)			// move into Rhythmic_head?
    dots_l_->position_i_ = position_i_;
}

IMPLEMENT_IS_TYPE_B1(Note_head,Rhythmic_head);


int
Note_head::compare (Note_head *const  &a, Note_head * const &b)
{
  return a->position_i_ - b->position_i_;
}

Interval
Note_head::do_width () const
{
  Atom a =  paper ()->lookup_l()->ball (balltype_i_);
  Interval i = a.dim_[X_AXIS];
  i+= x_dir_ * i.length ();
  return i;
}

Molecule*
Note_head::brew_molecule_p() const 
{
  Molecule*out = 0;
  Paper_def *p = paper();
  Real inter_f = p->internote_f ();

  // ugh
  bool streepjes_b = (position_i_<-1) || (position_i_ > staff_size_i_+1);
  
  Atom  s = p->lookup_l()->ball (balltype_i_);
  out = new Molecule (Atom (s));
  out->translate (x_dir_ * s.dim_[X_AXIS].length (), X_AXIS);

  if (streepjes_b) 
    {
      int dir = sign (position_i_);
      int s =(position_i_<-1) 
	? -((-position_i_)/2)
	: (position_i_-staff_size_i_)/2;
	
      Atom str = p->lookup_l()->streepjes (s);
      Molecule sm;
      sm.add (Atom (str));
      if (position_i_ % 2)
	sm.translate (-inter_f* dir, Y_AXIS);
      out->add (sm);	    
    }
  
  out->translate (inter_f*position_i_, Y_AXIS);
  return out;
}
