/*
  lyric-extender.cc -- implement Lyric_extender
  source file of the GNU LilyPond music typesetter

  (c)  1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys
*/

#include "dimension-cache.hh"
#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "lyric-extender.hh"


MAKE_SCHEME_SCORE_ELEMENT_CALLBACK(Lyric_extender,brew_molecule)
SCM 
Lyric_extender::brew_molecule (SCM smob) 
{
  Spanner *sp = dynamic_cast<Spanner*> (unsmob_element (smob));
  
  Real leftext = sp->get_bound (LEFT)->extent (X_AXIS).length ();
  Real ss = sp->paper_l ()->get_var ("staffspace");
  Real w = sp->spanner_length () - leftext - ss/2;
  
  Real h = sp->paper_l ()->get_var ("extender_height");
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

