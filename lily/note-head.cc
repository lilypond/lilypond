/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "misc.hh"
#include "dots.hh"
#include "note-head.hh"
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
  Atom a =  lookup_l ()->ball (balltype_i_);
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
  int streepjes_i = abs (position_i_) < staff_size_i_/2 
    ? 0
    : (abs(position_i_) - staff_size_i_/2) /2;
  
  Atom  s = lookup_l()->ball (balltype_i_);
  out = new Molecule (Atom (s));
  out->translate_axis (x_dir_ * s.dim_[X_AXIS].length (), X_AXIS);

  if (streepjes_i) 
    {
      int dir = sign (position_i_);
      Atom streepje = lookup_l ()->streepje (balltype_i_);
      
      int parity =  (position_i_ % 2) ? 1 : 0;
	
      
      for (int i=0; i < streepjes_i; i++)
	{
	  Atom s = streepje;
	  s.translate_axis (-dir * inter_f * (i*2 + parity),
			   Y_AXIS);
	  out->add_atom (s);
	}
    }
  
  out->translate_axis (inter_f*position_i_, Y_AXIS);
  return out;
}
