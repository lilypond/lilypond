/*

  text-spanner.cc -- implement Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "molecule.hh"
#include "text-item.hh"
#include "text-spanner.hh"
#include "line-spanner.hh"
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
 */

MAKE_SCHEME_CALLBACK (Text_spanner, brew_molecule, 1);

SCM
Text_spanner::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);



  /* Ugh, must be same as Hairpin::brew_molecule.  */
  Real padding = gh_scm2double (me->get_grob_property ("if-text-padding"));
  Real broken_left =  spanner->get_broken_left_end_align ();
  Real width = spanner->spanner_length ();
  width -= broken_left;

  Drul_array<bool> broken;
  Drul_array<Real> extra_off;
  Direction d = LEFT;
  do
    {
      extra_off [d]=0;
      Item *b = spanner->get_bound (d);
      broken[d] = b->break_status_dir () != CENTER;

      if (!broken [d])
	{

	  Interval e = b->extent (b, X_AXIS);
	  Real r = 0.0;
	  if (!e.empty_b ())
	    r = e[-d] + padding;
	  /* Text spanners such as ottava, should span from outer limits of
	   noteheads, iso (de)cresc. spanners that span the inner space */
	  if (me->get_grob_property ("outer") != SCM_EOL)
	    // r *= -1; // huh?
	    {
	      width -= d * r;
	    }
	  else
	    {
	      width += d * r;
	      extra_off[d] = r;
	    }
	}
    }
  while (flip (&d) != LEFT);

  // FIXME: ecs tells us -- only for (de)cresc. spanners
  width += gh_scm2double (me->get_grob_property ("width-correct"));
  /* /Ugh */


  SCM properties = Font_interface::font_alist_chain (me);

  SCM edge_text = me->get_grob_property ("edge-text");
  Drul_array<Molecule> edge;
  if (gh_pair_p (edge_text))
    {
      Direction d = LEFT;
      do
	{
	  /*  Don't repeat edge text for broken end */
	  if (!broken[d])
	    {
	      SCM text = index_cell (edge_text, d);
	      edge[d] = Text_item::text2molecule (me, text, properties);
	      if (!edge[d].empty_b ())
		edge[d].align_to (Y_AXIS, CENTER);
	    }
	}
      while (flip (&d) != LEFT);
    }
  width -= edge[LEFT].extent (X_AXIS).length ()
    + edge[RIGHT].extent (X_AXIS).length ();

  Drul_array<Real> shorten;
  shorten[LEFT] = 0;
  shorten[RIGHT] = 0;

  SCM s = me->get_grob_property ("shorten");
  if (gh_pair_p (s))
    {
      shorten[LEFT] = gh_scm2double (ly_car (s));
      shorten[RIGHT] = gh_scm2double (ly_cdr (s));
    }

  width -= shorten[LEFT] + shorten[RIGHT];
  
  if (width < 0)
    {
      warning (_ ("Text_spanner too small"));
      width = 0;
    }

  /* ugh */
  Real thick = me->paper_l ()->get_var ("stafflinethickness");  
  
  Molecule line = Line_spanner::line_molecule (me, width, 0);
  
  Drul_array<Molecule> edge_line;
  s = me->get_grob_property ("edge-height");
  if (gh_pair_p (s))
    {
      Direction d = LEFT;
      int dir = to_dir (me->get_grob_property ("direction"));
      do
	{
	  Real dy = gh_scm2double (index_cell (s, d)) * - dir;
	  if (dy)
	    {
	      SCM list = Line_spanner::line_atom (me, 0, dy);
	      Box b (Interval (0, thick),
		     dy > 0
		     ? Interval (0, dy)
		     : Interval (dy, 0));
	      edge_line[d] = Molecule (b, list);
	    }
	}
      while (flip (&d) != LEFT);
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
  m.translate_axis (broken_left + extra_off[LEFT], X_AXIS);

  return m.smobbed_copy ();
}


