/*
  lyric-extender.cc -- implement Lyric_extender
  source file of the GNU LilyPond music typesetter

  (c)  1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys
*/


#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "lyric-extender.hh"


MAKE_SCHEME_CALLBACK (Lyric_extender,brew_molecule,1)
SCM 
Lyric_extender::brew_molecule (SCM smob) 
{
  Spanner *sp = unsmob_spanner (smob);

  // ugh: refp
  Real leftext = sp->get_bound (LEFT)->extent (sp->get_bound (LEFT),
					       X_AXIS).length ();

  Real sl = sp->paper_l ()->get_var ("stafflinethickness");  
  Real righttrim = 0.5; // default to half a space gap on the right
  SCM righttrim_scm = sp->get_grob_property ("right-trim-amount");
  if (gh_number_p (righttrim_scm)) {
    righttrim = gh_scm2double (righttrim_scm);
  }
  // The extender can exist in the word space of the left lyric ...
  SCM space =  sp->get_bound (LEFT)->get_grob_property ("word-space");
  if (gh_number_p (space))
    {
      leftext -=  gh_scm2double (space);
    }
  Real w = sp->spanner_length () - leftext - righttrim;
  
  Real h = sl * gh_scm2double (sp->get_grob_property ("height"));
  Molecule  mol (Lookup::filledbox (Box (Interval (0,w), Interval (0,h))));
  mol.translate (Offset (leftext, 0));
  return mol.smobbed_copy ();
}

void
Lyric_extender::set_textitem (Spanner*sp, Direction d, Grob*s)
{
  sp->set_bound (d, s);
  sp->add_dependency (s);
}


