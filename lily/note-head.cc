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
Note_head::do_pre_processing ()
{
  // 8 ball looks the same as 4 ball:
  String type; 
  SCM style  = get_elt_property ("style");
  if (gh_string_p (style))
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

 String type; 
  SCM style  = get_elt_property ("style");
  if (style != SCM_UNDEFINED)
    {
      type = ly_scm2string (style);
    }
  
  Molecule*  out =
    new Molecule (lookup_l()->afm_find (String ("noteheads-") + to_str (balltype_i ()) + type));

  Box ledgerless = out->dim_;

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

  out->dim_ = ledgerless;
  return out;
}

