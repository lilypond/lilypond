/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "misc.hh"
#include "dots.hh"
#include "note-head.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "musical-request.hh"
#include "stem.hh"

void
Note_head::flip_around_stem (Direction d)
{
  translate_axis (do_width ().length () * d, X_AXIS);
}

Note_head::Note_head ()
{
}

void
Note_head::do_pre_processing ()
{
  Rhythmic_head::do_pre_processing ();
  // 8 ball looks the same as 4 ball:
  if (balltype_i_ > 2)
    balltype_i_ = 2;
  if (dots_l_)			// move into Rhythmic_head?
    dots_l_->position_i_ = int (position_f ());
}



int
Note_head::compare (Note_head *const  &a, Note_head * const &b)
{
  return sign(a->position_f () - b->position_f ());
}

/**
 Don't account for ledgerlines in the width.
 */
Interval
Note_head::do_width () const
{
  Molecule a =  lookup_l ()->notehead (balltype_i_, ""); // UGH
  Interval i = a.dim_[X_AXIS];
  return i;
}

Molecule*
Note_head::do_brew_molecule_p() const 
{
  Real inter_f = staff_line_leading_f ()/2;
  int sz = lines_i ()-1;

  int streepjes_i = abs (position_f ()) < sz 
    ? 0
    : (abs(position_f ()) - sz) /2;


  String type; 
  SCM style  =get_elt_property (style_scm_sym);
  if (style != SCM_BOOL_F)
    {
      type = ly_scm2string (SCM_CDR(style));
    }
  
  Molecule*  out = new Molecule (lookup_l()->notehead (balltype_i_, type));

  Box b = out->dim_;

  if (streepjes_i) 
    {
      Direction dir = (Direction)sign (position_f ());
      Interval hd = out->dim_[X_AXIS];
      Real hw = hd.length ()/4;
      
      Molecule ledger
	= lookup_l ()->ledger_line  (Interval (hd[LEFT] - hw,
					       hd[RIGHT] + hw));
      
      int parity =  abs(int (position_f ())) % 2;
      
      for (int i=0; i < streepjes_i; i++)
	{
	  Molecule s (ledger);
	  s.translate_axis (-dir * inter_f * (i*2 + parity),
			   Y_AXIS);
	  out->add_molecule (s);
	}
    }

  out->dim_ = b;
  return out;
}
