/*
  lyric-extender.cc -- implement Lyric_extender
  source file of the GNU LilyPond music typesetter

  (c) 1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys
*/

#include <math.h>

#include "box.hh"
#include "warn.hh"
#include "lookup.hh"
#include "stencil.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "lyric-extender.hh"
#include "note-head.hh"
#include "group-interface.hh"

bool
Lyric_extender::is_visible (Grob *gr)
{
  Spanner*me = dynamic_cast<Spanner*> (gr);

  SCM heads = me->get_property ("heads");
  int l = scm_ilength (heads);
  if (l == 0)
    return false;
  
  return true;
}

MAKE_SCHEME_CALLBACK (Lyric_extender,print,1)
SCM 
Lyric_extender::print (SCM smob) 
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
    It seems that short extenders are even lengthened to go past the
    note head, but haven't found a pattern in it yet. --hwn 1/1/04
    
   */
  SCM minlen =  me->get_property ("minimum-length");
  Real right_point
    = left_point + (robust_scm2double  (minlen,0));

  Spanner *orig = dynamic_cast<Spanner*> (me->original_);
  bool last_line = orig
    && (me->get_break_index () == orig->broken_intos_.size () - 2)
    && !Lyric_extender::is_visible (orig->broken_intos_.top ());
    

  if (heads.size ())
    right_point = right_point >? heads.top ()->extent (common, X_AXIS)[RIGHT];

  Real h = sl * robust_scm2double (me->get_property ("thickness"), 0);
  Real pad = 2* h;

  if (r->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    right_point = right_point <? (r->extent (common, X_AXIS)[LEFT] - pad);
  else if (Note_head::has_interface (r))
    ; 
  else if (!last_line)
    {
      /*
	run to end of line.
       */
      right_point = right_point >? (r->extent (common, X_AXIS)[LEFT] - pad);
    }
  
  if (isinf (right_point))
    {
      programming_error ("Right point of extender not defined?");
      right_point = r->relative_coordinate (common, X_AXIS);
    }  

  left_point += pad;

  Real w = right_point - left_point;

  if (w < 1.5 * h)
    return SCM_EOL;
  
  Stencil  mol (Lookup::round_filled_box (Box (Interval (0,w), Interval (0,h)),
					   0.8 * h));
  mol.translate_axis (left_point - me->relative_coordinate (common, X_AXIS), X_AXIS);
  return mol.smobbed_copy ();
}


ADD_INTERFACE (Lyric_extender,"lyric-extender-interface",
  "The extender is a simple line at the baseline of the lyric "
  " that helps show the length of a melissima (tied/slurred note).",
  "thickness heads");
