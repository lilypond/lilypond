/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "tie.hh"
#include "spanner.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "rhythmic-head.hh"
#include "bezier.hh"
#include "paper-column.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "bezier.hh"
#include "stem.hh"
#include "note-head.hh"
#include "tie-column.hh"
#include "grob-array.hh"
#include "tie-formatting-problem.hh"
#include "tie-configuration.hh"


int
Tie::compare (Grob *const &s1,
	      Grob *const &s2)
{
  return sign (Tie::get_position (s1) - Tie::get_position (s2));
}

void
Tie::set_head (Grob *me, Direction d, Grob *h)
{
  dynamic_cast<Spanner *> (me)->set_bound (d, h);
}

Grob *
Tie::head (Grob *me, Direction d)
{
  Item *it = dynamic_cast<Spanner*> (me)->get_bound (d);
  if (Note_head::has_interface (it))
    return it;
  else
    return 0;
}

int
Tie::get_column_rank (Grob *me, Direction d)
{
  Spanner *span = dynamic_cast<Spanner *> (me);
  Grob *h = head (me, d);
  if (!h)
    h = span->get_bound (d);

  Grob *col = dynamic_cast<Item *> (h)->get_column ();
  return Paper_column::get_rank (col);
}

int
Tie::get_position (Grob *me)
{
  Direction d = LEFT;
  do
    {
      Grob *h = head (me, d);
      if (h)
	return (int) Staff_symbol_referencer::get_position (h);
    }
  while (flip (&d) != LEFT);

  /*

  TODO: this is theoretically possible for ties across more than 2
  systems.. We should look at the first broken copy.
  
  */
  programming_error ("Tie without heads. Suicide");
  me->suicide ();
  return 0;
}

/*
  Default:  Put the tie oppositie of the stem [Wanske p231]

  In case of chords: Tie_column takes over

  The direction of the Tie is more complicated (See [Ross] p136 and
  further).

  (what about linebreaks? )
*/
Direction
Tie::get_default_dir (Grob *me)
{
  Drul_array<Grob*> stems;
  Direction d = LEFT;
  do
    {
      Grob *stem = head (me, d) ? Rhythmic_head::get_stem (head (me, d)) : 0;
      if (stem)
	stem = Stem::is_invisible (stem) ? 0 : stem;

      stems[d] = stem;
    }
  while (flip (&d)!= LEFT);
  
  if (stems[LEFT] && stems[RIGHT])
    {
      if (get_grob_direction (stems[LEFT]) == UP
	  && get_grob_direction (stems[RIGHT]) == UP)
	return DOWN;
    }
  else if (stems[LEFT] || stems[RIGHT])
    {
      Grob *s = stems[LEFT] ? stems[LEFT] : stems[RIGHT];
      return -get_grob_direction (s);
    }

  return UP;
}


MAKE_SCHEME_CALLBACK(Tie, calc_direction, 1);
SCM
Tie::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *yparent = me->get_parent (Y_AXIS);
  if (Tie_column::has_interface (yparent)
      && unsmob_grob_array (yparent->get_object ("ties"))
      && unsmob_grob_array (yparent->get_object ("ties"))->size () > 1)
    {
      /* trigger positioning. */
      (void) yparent->get_property ("positioning-done");
    }
  else
    set_grob_direction (me, Tie::get_default_dir (me));

  return SCM_UNSPECIFIED;
}


void
Tie::set_default_control_points (Grob *me_grob)
{
  Spanner *me = dynamic_cast<Spanner*> (me_grob);
  Grob *common  = me;
  common = me->get_bound (LEFT)->common_refpoint (common, X_AXIS); 
  common = me->get_bound (RIGHT)->common_refpoint (common, X_AXIS); 
  
  Tie_formatting_problem problem;
  problem.from_tie (me);
  Tie_specification spec = problem.get_tie_specification (0);
  spec.has_manual_dir_ = true;
  spec.manual_dir_ = get_grob_direction (me);
  
  Tie_configuration conf
    = problem.find_optimal_tie_configuration (spec);
  
  set_control_points (me, problem.common_x_refpoint (),
		      conf, problem.details_);
}

void
Tie::set_control_points (Grob *me,
			 Grob *common,
			 Tie_configuration const &conf,
			 Tie_details const &details
			 )
{
  Bezier b = conf.get_transformed_bezier (details);
  b.translate (Offset (- me->relative_coordinate (common, X_AXIS), 0));

  SCM controls = SCM_EOL;
  for (int i = 4; i--;)
    {
      if (!b.control_[i].is_sane ())
	programming_error ("Insane offset");
      controls = scm_cons (ly_offset2scm (b.control_[i]), controls);
    }
  me->set_property ("control-points", controls);
}

MAKE_SCHEME_CALLBACK(Tie, calc_control_points, 1);
SCM
Tie::calc_control_points (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  // trigger Tie-column
  (void)  get_grob_direction (me);

  Grob *yparent = me->get_parent (Y_AXIS);
  if (Tie_column::has_interface (yparent)
      && unsmob_grob_array (yparent->get_object ("ties"))
      && unsmob_grob_array (yparent->get_object ("ties"))->size () > 1)
    {
      /* trigger positioning. */
      (void) yparent->get_property ("positioning-done");
    }

  if (!scm_is_pair (me->get_property ("control-points")))
    {
      set_default_control_points (me);
    }

  return SCM_UNSPECIFIED;
}

/*
  TODO: merge witnh Slur::print.
 */
MAKE_SCHEME_CALLBACK (Tie, print, 1);
SCM
Tie::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  SCM cp = me->get_property ("control-points");

  Real staff_thick = Staff_symbol_referencer::line_thickness (me);
  Real base_thick = staff_thick * robust_scm2double (me->get_property ("thickness"), 1);
  Real line_thick = staff_thick * robust_scm2double (me->get_property ("line-thickness"), 1);

  Bezier b;
  int i = 0;
  for (SCM s = cp; s != SCM_EOL; s = scm_cdr (s))
    {
      b.control_[i] = ly_scm2offset (scm_car (s));
      i++;
    }

  Stencil a;

  SCM p = me->get_property ("dash-period");
  SCM f = me->get_property ("dash-fraction");
  if (scm_is_number (p) && scm_is_number (f))
    a = Lookup::dashed_slur (b,
			     line_thick,
			     robust_scm2double (p, 1.0),
			     robust_scm2double (f, 0));
  else
    a = Lookup::slur (b,
		      get_grob_direction (me) * base_thick,
		      line_thick);

  return a.smobbed_copy ();
}

ADD_INTERFACE (Tie,
	       "tie-interface",
	       
	       "A horizontal curve connecting two noteheads. \n\n"
	       ,
	       

	       /* properties */
	       "avoid-slur " 	//  UGH.
	       "control-points "
	       "dash-fraction "
	       "dash-period "
	       "details "
	       "direction "
	       "line-thickness " 
	       "thickness "
	       );




