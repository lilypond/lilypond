/*
  lyric-extender.cc -- implement Lyric_extender
  source file of the GNU LilyPond music typesetter

  (c)  1998--2003 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys
*/

#include <math.h>

#include "box.hh"
#include "warn.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "lyric-extender.hh"
#include "note-head.hh"
#include "group-interface.hh"

MAKE_SCHEME_CALLBACK (Lyric_extender,brew_molecule,1)
SCM 
Lyric_extender::brew_molecule (SCM smob) 
{
  Spanner *me = unsmob_spanner (smob);
  Item *l = me->get_bound (LEFT);
  Item *r = me->get_bound (RIGHT);
  Grob *common = l->common_refpoint (r, X_AXIS);
  

  Real sl = me->get_paper ()->get_realvar (ly_symbol2scm ("linethickness"));  

  Link_array<Grob> heads (Pointer_group_interface__extract_grobs (me, (Grob*)0,
								  "heads"));

  if (!heads.size ())
    return SCM_EOL;

  common = common_refpoint_of_array (heads, common, X_AXIS);
  
  Real left_point = 0.0;
  if (l->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    left_point = l->extent (common, X_AXIS)[RIGHT];
  else
    left_point = heads[0]->extent (common, X_AXIS)[LEFT];


  if (isinf (left_point))
    return SCM_EOL;
  

  /*
    It seems that short extenders are even lengthened to go past the note head,  but
    haven't found a pattern in it yet. --hwn  1/1/04
    
   */
  
  SCM minlen =  me->get_grob_property ("minimum-length");
  Real right_point
    = left_point + (gh_number_p (minlen) ? gh_scm2double (minlen) : 0.0);

  right_point = right_point >? heads.top ()->extent (common, X_AXIS)[RIGHT];

  Real h = sl * gh_scm2double (me->get_grob_property ("thickness"));
  Real pad = 2* h;
  right_point = right_point <? (r->extent (common, X_AXIS)[LEFT] - pad);

  if (isinf (right_point))
    return SCM_EOL;
  

  right_point += pad;

  Real w = right_point - left_point;

  if (w < 0)
    return SCM_EOL;
  
  Molecule  mol (Lookup::round_filled_box (Box (Interval (0,w), Interval (0,h)),
					   0.8 * h));
  mol.translate_axis (left_point - me->relative_coordinate (common, X_AXIS), X_AXIS);
  return mol.smobbed_copy ();
}


ADD_INTERFACE (Lyric_extender,"lyric-extender-interface",
  "The extender is a simple line at the baseline of the lyric "
  " that helps show the length of a melissima (tied/slurred note).",
  "thickness heads");
