/*
  text-spanner.cc -- implement Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "molecule.hh"
#include "text-spanner.hh"
#include "spanner.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"

/*
  Generic Text spanner:

  type: "line", "dashed-line", "dotted-line"
  text: "text"
  edge-text: ("le" . "re")
  text-style: "italic"

     not-yet-text
  le -------------- re

  TODO:
    - vertical start / vertical end (fixme-name) |
    - contination types (vert. star, vert. end)  |-> eat volta-spanner
    - more styles
    - more texts/positions
    - style: hairpin ?
 */

MAKE_SCHEME_CALLBACK (Text_spanner, brew_molecule, 1);

SCM
Text_spanner::brew_molecule (SCM smob) 
{
  Score_element *me= unsmob_element (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real line = me->paper_l ()->get_var ("stafflinethickness");  
  

  Drul_array<bool> broken;
  Direction d = LEFT;
  do
    {
      Paper_column* s = dynamic_cast<Paper_column*>(spanner->get_bound (d)); // UGH
      broken[d] = (!s->musical_b ());
    }
  while (flip (&d) != LEFT);
  
  SCM s = me->get_elt_property ("text-style");
  String text_style = "italic";
  if (gh_string_p (s))
    text_style = ly_scm2string (s);
 
  SCM edge_text = me->get_elt_property ("edge-text");
  Drul_array<Molecule> edge;
  if (gh_pair_p (edge_text))
    {
      edge[LEFT] = me->lookup_l ()->text (text_style,
					  ly_scm2string (gh_car (edge_text)),
					  me->paper_l ());
      if (!edge[LEFT].empty_b ())
	edge[LEFT].align_to (Y_AXIS, CENTER);
      edge[RIGHT] = me->lookup_l ()->text (text_style,
					   ly_scm2string (gh_cdr (edge_text)),
					   me->paper_l ());
      if (!edge[RIGHT].empty_b ())
	edge[RIGHT].align_to (Y_AXIS, CENTER);
    }

  Drul_array<Real> shorten;
  shorten[LEFT] = 0;
  shorten[RIGHT] = 0;

  s = me->get_elt_property ("shorten");
  if (gh_pair_p (s))
    {
      shorten[LEFT] = gh_scm2double (gh_car (s)) * staff_space;
      shorten[RIGHT] = gh_scm2double (gh_cdr (s)) * staff_space;
    }

  Real broken_left =  spanner->get_broken_left_end_align ();
  Real width = spanner->spanner_length ();
  width += spanner->get_bound (RIGHT)->extent (X_AXIS).length ();
  width -= broken_left;
  width -= shorten[LEFT] + shorten[RIGHT];
  width -= edge[LEFT].extent (X_AXIS).length ()
    + edge[RIGHT].extent (X_AXIS).length ();

  if (width < 0)
    {
      warning (_ ("Text_spanner too small"));
      width = 0;
    }


  String type = "dashed-line";
  s = me->get_elt_property ("type");
  if (gh_string_p (s))
    type = ly_scm2string (s);

  Real height;
  SCM at;
  if (type == "line"
      || type == "dashed-line"
      || type == "dotted-line")
    {
      Real thick = line;
      s = me->get_elt_property ("line-thickness");
      if (gh_number_p (s))
	thick *= gh_scm2double (s);
  
      // maybe these should be in line-thickness?
      Real length = staff_space;
      s = me->get_elt_property ("dash-length");
      if (gh_number_p (s))
	length = gh_scm2double (s) * staff_space;

      Real period = 2 * length + thick;
      s = me->get_elt_property ("dash-period");
      if (gh_number_p (s))
	period = gh_scm2double (s) * staff_space;
      
      if (type == "dotted-line")
	length = thick;
	
      if (type == "line")
	length = period + thick;

      Real on = length - thick;
      Real off = period - on;

      height = thick;
      at = gh_list (ly_symbol2scm ("dashed-line"),
		    gh_double2scm (thick),
		    gh_double2scm (on),
		    gh_double2scm (off),
		    gh_double2scm (width),
		    SCM_UNDEFINED);
    }
  Box b (Interval (0, width), Interval (-height / 2, height / 2));
  Molecule span (b, at);

  Molecule m;
  if (!edge[LEFT].empty_b ())
    m = edge[LEFT];

  if (!span.empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, span, 0);
  if (!edge[RIGHT].empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, edge[RIGHT], 0);
  m.translate_axis (broken_left, X_AXIS);

  return m.create_scheme ();
}


