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

void
Crescendo::set_interface (Score_element*s)
{
  s->set_elt_property ("dynamic-drul", gh_cons (SCM_UNDEFINED, SCM_UNDEFINED));
}



MAKE_SCHEME_CALLBACK(Crescendo,brew_molecule);
SCM
Crescendo::brew_molecule (SCM smob) 
{
  Score_element *me= unsmob_element (smob);
  Spanner * sp = dynamic_cast<Spanner*>(me);
  Real absdyn_dim = me->paper_l ()-> get_var ("crescendo_shorten");
  Real extra_left =  sp->get_broken_left_end_align ();

  SCM dir = me->get_elt_property("grow-direction");
  SCM dyns = me->get_elt_property ("dynamic-drul");

  if (!isdir_b (dir) || !gh_pair_p (dyns))
    {
      me->suicide ();
      return SCM_EOL;
    }
  
  Direction gd = to_dir (dir);

  bool dynleft= unsmob_element (gh_car (dyns));
  bool dynright = unsmob_element (gh_cdr (dyns));
  
  if (dynleft)
    extra_left += absdyn_dim;

  Real width = sp->spanner_length()- sp->get_broken_left_end_align ();

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
      Paper_column* s = dynamic_cast<Paper_column*>(sp->get_bound (d)); // UGH
      broken[d] = (!s->musical_b ());
    }
  while (flip (&d) != LEFT);
  
 
  Molecule m;
  
  Real pad = 0;
  SCM s = me->get_elt_property ("start-text");
  if (gh_string_p (s))
    {
      Molecule start_text (me->lookup_l ()->text ("italic",
					      ly_scm2string (s),
						  me->paper_l ()));
      m.add_molecule (start_text);

      pad = me->paper_l ()->get_var ("interline") / 2;

      width -= start_text.extent (X_AXIS).length ();
      width -= pad;
      width = width >? 0;
    }

  SCM at;
  s =me->get_elt_property ("spanner");
  Real height;
  if (gh_string_p (s) && ly_scm2string (s) == "dashed-line")
    {
      Real thick = me->paper_l ()->get_var ("crescendo_dash_thickness");
      Real dash = me->paper_l ()->get_var ("crescendo_dash");
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
      height = me->paper_l()->get_var ("crescendo_height");
      Real thick = me->paper_l ()->get_var ("crescendo_thickness");
      
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


