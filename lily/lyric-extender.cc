/*
  lyric-extender.cc -- implement Lyric_extender
  source file of the GNU LilyPond music typesetter

  (c)  1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys
*/


#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "lyric-extender.hh"


MAKE_SCHEME_CALLBACK(Lyric_extender,brew_molecule,1)
SCM 
Lyric_extender::brew_molecule (SCM smob) 
{
  Spanner *sp = dynamic_cast<Spanner*> (unsmob_element (smob));

  // ugh: refp
  Real leftext = sp->get_bound (LEFT)->extent (sp->get_bound (LEFT),
					       X_AXIS).length ();
  Real ss = sp->paper_l ()->get_var ("staffspace");
  Real sl = sp->paper_l ()->get_var ("stafflinethickness");  
  Real righttrim = 0.5; // default to half a staffspace gap on the right
  SCM righttrim_scm = sp->get_elt_property("right-trim-amount");
  if (gh_number_p (righttrim_scm)) {
    righttrim = gh_scm2double (righttrim_scm);
  }
  // The extender can exist in the word space of the left lyric ...
  SCM space =  sp->get_bound (LEFT)->get_elt_property ("word-space");
  if (gh_number_p (space))
    {
      leftext -=  gh_scm2double (space)*ss;
    }
  Real w = sp->spanner_length () - leftext - righttrim*ss;
  
  Real h = sl * gh_scm2double (sp->get_elt_property  ("height"));
  Molecule  mol (sp->lookup_l ()->filledbox ( Box (Interval (0,w), Interval (0,h))));
  mol.translate (Offset (leftext, 0));
  return mol.create_scheme();
}

void
Lyric_extender::set_textitem (Direction d, Score_element*s)
{
  elt_l_->set_bound (d, s);
  elt_l_->add_dependency (s);
}

Lyric_extender::Lyric_extender (Spanner*s)
{
  elt_l_ = s;
}

