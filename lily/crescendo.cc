/*
  crescendo.cc -- implement Crescendo

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "crescendo.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "paper-column.hh"
#include "atom.hh"

Crescendo::Crescendo ()
{
  set_elt_property ("dynamic-drul", gh_cons (SCM_BOOL_F, SCM_BOOL_F));
}



Molecule*
Crescendo::do_brew_molecule_p () const
{
  Real absdyn_dim = paper_l ()-> get_var ("crescendo_shorten");
  Real extra_left =  get_broken_left_end_align ();

  SCM dir = get_elt_property("grow-direction");
  SCM dyns = get_elt_property ("dynamic-drul");

  if (!isdir_b (dir) || !gh_pair_p (dyns))
    {
Crescendo * me = (Crescendo*)this;
      me->set_elt_property ("transparent", SCM_BOOL_T);
      return new Molecule;
    }
  
  Direction gd = to_dir (dir);

  bool dynleft= to_boolean (gh_car (dyns));
  bool dynright = to_boolean (gh_cdr (dyns));
  
  if (dynleft)
    extra_left += absdyn_dim;

  

  Real width = spanner_length()- get_broken_left_end_align ();

  if (dynleft)
    {
      width -= absdyn_dim;
    }
  if (dynright)
    {
      width -= absdyn_dim;
    }

  if (width < 0)
    {
      warning (_ ("crescendo") + " " + _ ("too small"));
      width = 0;
    }

  Drul_array<bool> broken;
  Direction d = LEFT;
  do {
    Paper_column* s = dynamic_cast<Paper_column*>(spanned_drul_[d]); // UGH
    broken[d] = (!s->musical_b ());
  } while (flip (&d) != LEFT);
  

  bool continued = broken[Direction (-gd)];
  Real height = paper_l()->get_var ("crescendo_height");
  Real thick = paper_l ()->get_var ("crescendo_thickness");

  const char* hairpin = (gd < 0)? "decrescendo" :  "crescendo";
  Molecule * m
    = new Molecule;
  m->dim_.x () = Interval (0, width);
  m->dim_.y () = Interval (-2*height, 2*height);

  SCM at = gh_list (ly_symbol2scm (hairpin),
		     gh_double2scm (thick),
		     gh_double2scm (width),
		     gh_double2scm (height),
		     gh_double2scm (continued ? height/2 : 0.0),
		     SCM_UNDEFINED);

  m->add_atom (at);
  m->translate_axis (extra_left, X_AXIS);
  return m;
}


