/*
  text-spanner.cc -- implement Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "molecule.hh"
#include "text-item.hh"
#include "text-spanner.hh"
#include "spanner.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"

/*
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
  Real thickness = me->paper_l ()->get_var ("stafflinethickness");  
  

  Drul_array<bool> broken;
  Direction d = LEFT;
  do
    {
      Paper_column* s = dynamic_cast<Paper_column*>(spanner->get_bound (d)); // UGH
      broken[d] = (!s->musical_b ());
    }
  while (flip (&d) != LEFT);
  
#if 0
  SCM s = me->get_elt_property ("text-style");

  String text_style = "italic";
  if (gh_string_p (s))
    text_style = ly_scm2string (s);
#endif

  SCM properties = Font_interface::font_alist_chain (me);

  SCM edge_text = me->get_elt_property ("edge-text");
  Drul_array<Molecule> edge;
  if (gh_pair_p (edge_text))
    {
      Direction d = LEFT;
      do
	{
	  SCM text = index_cell (edge_text, d);
	  edge[d] = Text_item::text2molecule (me, text, properties);
	  if (!edge[d].empty_b ())
	    edge[d].align_to (Y_AXIS, CENTER);
	}
      while (flip (&d) != LEFT);
    }

  Drul_array<Real> shorten;
  shorten[LEFT] = 0;
  shorten[RIGHT] = 0;

  SCM s = me->get_elt_property ("shorten");
  if (gh_pair_p (s))
    {
      shorten[LEFT] = gh_scm2double (gh_car (s)) * staff_space;
      shorten[RIGHT] = gh_scm2double (gh_cdr (s)) * staff_space;
    }

  Real broken_left =  spanner->get_broken_left_end_align ();
  Real width = spanner->spanner_length ();
  Score_element *bnd = spanner->get_bound (RIGHT);
  width += bnd->extent (bnd, X_AXIS).length ();
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

  Molecule line;
  Drul_array<Molecule> edge_line;
  if (type == "line"
      || type == "dashed-line"
      || type == "dotted-line")
    {
      Real thick = thickness;
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

      SCM list = gh_list (ly_symbol2scm ("dashed-line"),
			  gh_double2scm (thick),
			  gh_double2scm (on),
			  gh_double2scm (off),
			  gh_double2scm (width),
			  gh_double2scm (0),
			  SCM_UNDEFINED);

      Box b (Interval (0, width), Interval (-thick / 2, thick / 2));
      line = Molecule (b, list);

      s = me->get_elt_property ("edge-height");
      if (gh_pair_p (s))
	{
	  Direction d = LEFT;
	  int dir = to_dir (me->get_elt_property ("direction"));
	  do
	    {
	      Real dy = gh_scm2double (index_cell (s, d)) * - dir;
	      if (dy)
		{
		  SCM list = gh_list (ly_symbol2scm ("dashed-line"),
				      gh_double2scm (thick),
				      gh_double2scm (on),
				      gh_double2scm (off),
				      gh_double2scm (0),
				      gh_double2scm (dy),
				      SCM_UNDEFINED);
		  
		  Box b (Interval (0, thick),
			 dy > 0
			 ? Interval (0, dy)
			 : Interval (dy, 0));
		  edge_line[d] = Molecule (b, list);
		}
	    }
	  while (flip (&d) != LEFT);
	}
    }

  Molecule m;
  if (!edge[LEFT].empty_b ())
    m = edge[LEFT];

  if (!edge_line[LEFT].empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, edge_line[LEFT], 0);
  if (!line.empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, line, 0);
  if (!edge_line[RIGHT].empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, edge_line[RIGHT], 0);
  if (!edge[RIGHT].empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, edge[RIGHT], 0);
  m.translate_axis (broken_left, X_AXIS);

  return m.smobbed_copy ();
}


