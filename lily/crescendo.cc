/*
  crescendo.cc -- implement Crescendo

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "crescendo.hh"
#include "spanner.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "paper-column.hh"

MAKE_SCHEME_CALLBACK (Crescendo, brew_molecule, 1);

SCM
Crescendo::brew_molecule (SCM smob) 
{
  Score_element *me= unsmob_element (smob);
  Spanner *span = dynamic_cast<Spanner*>(me);
  Real staff_space = me->paper_l ()->get_var ("staffspace");
  Real line = me->paper_l ()->get_var ("stafflinethickness");  
  
  Real broken_left =  span->get_broken_left_end_align ();

  SCM s = me->get_elt_property("grow-direction");
  if (!isdir_b (s))
    {
      me->suicide ();
      return SCM_EOL;
    }
  
  Direction grow_dir = to_dir (s);

  Real width = span->spanner_length ();
  width -= span->get_broken_left_end_align ();

  if (width < 0)
    {
      warning (_ ((grow_dir < 0) ? "decrescendo too small"
		  : "crescendo too small"));
      width = 0;
    }

  Drul_array<bool> broken;
  Direction d = LEFT;
  do
    {
      Paper_column* s = dynamic_cast<Paper_column*> (span->get_bound (d)); // UGH
      broken[d] = (!s->musical_b ());
    }
  while (flip (&d) != LEFT);
  
  bool continued = broken[Direction (-grow_dir)];
  Real height = staff_space * gh_scm2double (me->get_elt_property ("height"));
  Real thick = line * gh_scm2double (me->get_elt_property ("thickness"));
  
  const char* type = (grow_dir < 0) ? "decrescendo" :  "crescendo";
  SCM hairpin = gh_list (ly_symbol2scm (type),
		    gh_double2scm (thick),
		    gh_double2scm (width),
		    gh_double2scm (height),
		    gh_double2scm (continued ? height/2 : 0.0),
		    SCM_UNDEFINED);

  Box b (Interval (0, width), Interval (-2*height, 2*height));
  Molecule mol (b, hairpin);
  mol.translate_axis (broken_left, X_AXIS);

  return mol.create_scheme ();
}


