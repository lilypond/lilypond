/*
  text-spanner.cc -- implement Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2003 Jan Nieuwenhuizen <janneke@gnu.org>

  Revised over good by Han-Wen. 
*/

#include "molecule.hh"
#include "text-item.hh"
#include "text-spanner.hh"
#include "line-spanner.hh"
#include "spanner.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"

/*
  TODO:
  - vertical start / vertical end (fixme-name) |
  - contination types (vert. star, vert. end)  |-> eat volta-bracket
  - more styles
  - more texts/positions
*/

MAKE_SCHEME_CALLBACK (Text_spanner, brew_molecule, 1);

/*
  TODO: this function is too long
*/
SCM
Text_spanner::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);
  
  /* Ugh, must be same as Hairpin::brew_molecule.  */
  Real padding = 0.0;
  SCM itp= me->get_grob_property ("if-text-padding");
  if (gh_number_p (itp))
    padding = gh_scm2double (itp);

  Grob *common = spanner->get_bound (LEFT)->common_refpoint (spanner->get_bound (RIGHT), X_AXIS);
  
  Interval span_points;
  Drul_array<bool> broken;
  Direction d = LEFT;
  do
    {
      Item *b = spanner->get_bound (d);
      broken[d] = b->break_status_dir () != CENTER;

      if (broken[d])
	{
	if (d == LEFT)
	  span_points[d] = spanner->get_broken_left_end_align ();
	else
	  span_points[d] = b->relative_coordinate (common, X_AXIS);
	}
      else
	  {
	    bool encl = to_boolean (me->get_grob_property ("enclose-bounds"));
	    span_points[d] = b->extent (common, X_AXIS)[encl ? d : -d];
	  }
    }
  while (flip (&d) != LEFT);



  SCM properties = Font_interface::font_alist_chain (me);
  SCM edge_text = me->get_grob_property ("edge-text");
  Drul_array<Molecule> edge;
  if (gh_pair_p (edge_text))
    {
      Direction d = LEFT;
      do
	{
	  /*  Don't repeat edge text for broken end */
	  if (broken[d])
	    continue;
	  
	  SCM text = index_get_cell (edge_text, d);

	  /*
	    TODO: use markup.
	   */
	  
	  edge[d] = Text_item::interpret_new_markup (smob, properties, text);
	  if (!edge[d].empty_b ())
	    edge[d].align_to (Y_AXIS, CENTER);
	}
      while (flip (&d) != LEFT);
    }


  Drul_array<Real> shorten;
  shorten[LEFT] = 0;
  shorten[RIGHT] = 0;

  SCM ew = me->get_grob_property ("bracket-flare");
  SCM s = me->get_grob_property ("shorten-pair");
  if (gh_pair_p (s))
    {
      span_points[LEFT] += gh_scm2double (ly_car (s));
      span_points[RIGHT] -= gh_scm2double (ly_cdr (s));
    }
  if (gh_pair_p (ew))
    {
      span_points[LEFT] += gh_scm2double (ly_car (ew));
      span_points[RIGHT] -= gh_scm2double (ly_cdr (ew));
    }
  
  Real thick = me->get_paper ()->get_realvar (ly_symbol2scm ("linethickness"));  
  SCM st = me->get_grob_property ("thickness");
  if (gh_number_p (st))
    {
      thick *=  gh_scm2double (st);
    }
  
  Drul_array<Molecule> edge_line;
  s = me->get_grob_property ("edge-height");
  if (gh_pair_p (s))
    {
      Direction d = LEFT;
      int dir = to_dir (me->get_grob_property ("direction"));
      do
	{
	  if (broken[d])
	    continue;
	  
	  Real dx = 0.0;
	  if (gh_pair_p (ew))
	    dx = gh_scm2double (index_get_cell (ew, d)) * d;

	  Real dy = gh_scm2double (index_get_cell (s, d)) * - dir;
	  if (dy)
	    edge_line[d] = Line_spanner::line_molecule (me, thick, Offset(0,0),
							Offset (dx, dy));
	}
      while (flip (&d) != LEFT);
    }
  
  Molecule m;
  do
    {
      Interval ext = edge[d].extent (X_AXIS);

      edge[d].translate_axis (span_points[d], X_AXIS);
      m.add_molecule (edge[d]);
      edge_line[d].translate_axis (span_points[d], X_AXIS);
      m.add_molecule (edge_line[d]);
      if (!ext.empty_b ())
	span_points[d] += -d *  ext[-d];
    }
  while (flip (&d) != LEFT);

  Molecule l =Line_spanner::line_molecule (me, thick,
					   Offset (span_points[LEFT], 0),
					   Offset (span_points[RIGHT], 0));
  m.add_molecule (l);

  m.translate_axis (- me->relative_coordinate (common, X_AXIS), X_AXIS);
  return m.smobbed_copy ();
}




ADD_INTERFACE (Text_spanner,"text-spanner-interface",
	       "generic text spanner",
	       "dash-period if-text-padding dash-length edge-height bracket-flare edge-text shorten-pair style thickness enclose-bounds width-correct");

