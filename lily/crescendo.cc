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


Crescendo::Crescendo (SCM s)
  : Spanner (s)
{
  set_elt_property ("dynamic-drul", gh_cons (SCM_BOOL_F, SCM_BOOL_F));
}




GLUE_SCORE_ELEMENT(Crescendo,brew_molecule);

SCM
Crescendo::member_brew_molecule () const
{
  Real absdyn_dim = paper_l ()-> get_var ("crescendo_shorten");
  Real extra_left =  get_broken_left_end_align ();

  SCM dir = get_elt_property("grow-direction");
  SCM dyns = get_elt_property ("dynamic-drul");

  if (!isdir_b (dir) || !gh_pair_p (dyns))
    {
      Crescendo * me = (Crescendo*)this;
      me->suicide ();
      return SCM_EOL;
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
  do
    {
      Paper_column* s = dynamic_cast<Paper_column*>(get_bound (d)); // UGH
      broken[d] = (!s->musical_b ());
    }
  while (flip (&d) != LEFT);
  
 
  Molecule m;
  
  Real pad = 0;
  SCM s = get_elt_property ("start-text");
  if (gh_string_p (s))
    {
      Molecule start_text (lookup_l ()->text ("italic",
					      ly_scm2string (s),
					      paper_l ()));
      m.add_molecule (start_text);

      pad = paper_l ()->get_var ("interline") / 2;

      width -= start_text.extent (X_AXIS).length ();
      width -= pad;
      width = width >? 0;
    }

  SCM at;
  s = get_elt_property ("spanner");
  Real height;
  if (gh_string_p (s) && ly_scm2string (s) == "dashed-line")
    {
      Real thick = paper_l ()->get_var ("crescendo_dash_thickness");
      Real dash = paper_l ()->get_var ("crescendo_dash");
      height = thick;
      at = gh_list (ly_symbol2scm (ly_scm2string (s).ch_C ()),
		    gh_double2scm (thick),
		    gh_double2scm (dash),
		    gh_double2scm (width),
		    SCM_UNDEFINED);
    }
  else
    {
      bool continued = broken[Direction (-gd)];
      height = paper_l()->get_var ("crescendo_height");
      Real thick = paper_l ()->get_var ("crescendo_thickness");
      
      const char* hairpin = (gd < 0)? "decrescendo" :  "crescendo";

      at = gh_list (ly_symbol2scm (hairpin),
		    gh_double2scm (thick),
		    gh_double2scm (width),
		    gh_double2scm (height),
		    gh_double2scm (continued ? height/2 : 0.0),
		    SCM_UNDEFINED);
    }
  Box b (Interval (0, width), Interval (-2*height, 2*height));
  Molecule span (b, at);

  m.add_at_edge (X_AXIS, RIGHT, span, pad);
  m.translate_axis (extra_left, X_AXIS);

  return m.create_scheme ();
}


