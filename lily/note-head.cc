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
#include "dimension-cache.hh"
#include "staff-symbol-referencer.hh"

void
Note_head::flip_around_stem (Direction d)
{
  Real l= make_molecule ().dim_[X_AXIS].length ();
  translate_axis (l * d, X_AXIS);
}

Interval
Note_head::dim_callback (Dimension_cache const * c)
{
  Note_head* n = dynamic_cast<Note_head*> (c->element_l ());
  return n->make_molecule ().dim_[X_AXIS];
}

Note_head::Note_head ()
{
  dim_cache_[X_AXIS]->callback_l_ = dim_callback;
}

void
Note_head::do_pre_processing ()
{
  // 8 ball looks the same as 4 ball:
  String type; 
  SCM style  = get_elt_property ("style");
  if (style != SCM_UNDEFINED)
    {
      type = ly_scm2string (style);
    }
  
  
  if (balltype_i () > 2 || type == "harmonic" || type == "cross")
    set_elt_property ("duration-log", gh_int2scm (2));

  if (Dots *d = dots_l ())
    { // move into Rhythmic_head?

      Staff_symbol_referencer_interface si (d);
      Staff_symbol_referencer_interface me (this);      
      
      si.set_position(int (me.position_f ()));
    }
}

int
Note_head::compare (Note_head *const  &a, Note_head * const &b)
{
  Staff_symbol_referencer_interface s1(a);
  Staff_symbol_referencer_interface s2(b);      

  return sign(s1.position_f () - s2.position_f ());
}

Molecule
Note_head::make_molecule () const
{
  String type; 
  SCM style  = get_elt_property ("style");
  if (style != SCM_UNDEFINED)
    {
      type = ly_scm2string (style);
    }
  
  return lookup_l()->afm_find (String ("noteheads-")
			       + to_str (balltype_i ()) + type);
}

Molecule*
Note_head::do_brew_molecule_p() const 
{
  Staff_symbol_referencer_interface si (this);
  
  Real inter_f = si.staff_line_leading_f ()/2;
  int sz = si.lines_i ()-1;
  Real p = si.position_f ();
  int streepjes_i = abs (p) < sz 
    ? 0
    : (abs((int)p) - sz) /2;

  Molecule*  out =  new Molecule (make_molecule ());

  Box b = out->dim_;

  if (streepjes_i) 
    {
      Direction dir = (Direction)sign (p);
      Interval hd = out->dim_[X_AXIS];
      Real hw = hd.length ()/4;
      
      Molecule ledger
	= lookup_l ()->ledger_line  (Interval (hd[LEFT] - hw,
					       hd[RIGHT] + hw));
      
      int parity =  abs(int (p)) % 2;
      
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

