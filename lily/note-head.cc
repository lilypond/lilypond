/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  steps_i_ = 0;
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




int
Note_head::compare (Note_head *const  &a, Note_head * const &b)
{
  return a->position_i_ - b->position_i_;
}

Interval
Note_head::do_width () const
{
  Molecule a =  lookup_l ()->ball (balltype_i_);
  Interval i = a.dim_[X_AXIS];
  i+= x_dir_ * i.length ();
  return i;
}

Molecule*
Note_head::do_brew_molecule_p() const 
{
  Molecule*out = 0;
  Paper_def *p = paper();
  Real inter_f = p->internote_f ();

  // ugh
  int streepjes_i = abs (position_i_) < staff_size_i_/2 
    ? 0
    : (abs(position_i_) - staff_size_i_/2) /2;
  
  Molecule head; 

  if (note_head_type_str_.length_i ()) {
    if (note_head_type_str_ == "normal") // UGH
      note_head_type_str_ = "";
    head = lookup_l()->special_ball (balltype_i_, note_head_type_str_);
    }
  else
    head = lookup_l()->ball (balltype_i_);
  
  out = new Molecule (Molecule (head));
  out->translate_axis (x_dir_ * head.dim_[X_AXIS].length (), X_AXIS);



  if (streepjes_i) 
    {
      Direction dir = sign (position_i_);
      Interval hd = head.dim_[X_AXIS];
      Real hw = hd.length ()/4;
      
      Molecule ledger
	= lookup_l ()->ledger_line  (Interval (hd[LEFT] - hw,
					       hd[RIGHT] + hw));
      
      int parity =  abs(position_i_) % 2;
      
      for (int i=0; i < streepjes_i; i++)
	{
	  Molecule s (ledger);
	  s.translate_axis (-dir * inter_f * (i*2 + parity),
			   Y_AXIS);
	  out->add_molecule (s);
	}
    }
  
  out->translate_axis (inter_f*position_i_, Y_AXIS);
  return out;
}
