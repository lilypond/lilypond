/*
  lyric-extender.cc -- implement Lyric_extender
  source file of the GNU LilyPond music typesetter

  (c)  1998--2003 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys
*/


#include "box.hh"
#include "warn.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "lyric-extender.hh"
#include "note-head.hh"

MAKE_SCHEME_CALLBACK (Lyric_extender,brew_molecule,1)
SCM 
Lyric_extender::brew_molecule (SCM smob) 
{
  Spanner *me = unsmob_spanner (smob);
  Item *l = me->get_bound (LEFT);
  Item *r = me->get_bound (RIGHT);
  Grob *common = l->common_refpoint (r, X_AXIS);
  
  Real left_point = l->extent (common, X_AXIS)[RIGHT];

  Real sl = me->get_paper ()->get_realvar (ly_symbol2scm ("linethickness"));  


  /*
    It seems that short extenders are even lengthened to go past the note head,  but
    haven't found a pattern in it yet. --hwn  1/1/04
    
   */
  Real right_point = r->extent (common, X_AXIS)
    [(Note_head::has_interface (r)) ? RIGHT : LEFT];
  
  Real w = right_point - left_point;

  Real h = sl * gh_scm2double (me->get_grob_property ("thickness"));
  
  Molecule  mol (Lookup::round_filled_box (Box (Interval (0,w), Interval (0,h)),
					   0.8 * h));
  mol.translate_axis (left_point - me->relative_coordinate (common, X_AXIS), X_AXIS);
  return mol.smobbed_copy ();
}

void
Lyric_extender::set_textitem (Spanner *me, Direction d, Grob *s)
{
  me->set_bound (d, s);
  me->add_dependency (s);
}




ADD_INTERFACE (Lyric_extender,"lyric-extender-interface",
  "The extender is a simple line at the baseline of the lyric "
  " that helps show the length of a melissima (tied/slurred note).",
  "thickness");
