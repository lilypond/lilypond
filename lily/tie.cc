/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "tie.hh"

#include "main.hh"
#include "bezier.hh"
#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "grob-array.hh"
#include "lookup.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "text-interface.hh"
#include "tie-column.hh"
#include "tie-configuration.hh"
#include "tie-formatting-problem.hh"
#include "warn.hh"
#include "semi-tie-column.hh"


bool
Tie::less (Grob *const &s1, Grob *const &s2)
{
  return Tie::get_position (s1) < Tie::get_position (s2);
}

void
Tie::set_head (Grob *me, Direction d, Grob *h)
{
  dynamic_cast<Spanner *> (me)->set_bound (d, h);
}

Grob *
Tie::head (Grob *me, Direction d)
{
  if (is_direction (me->get_property ("head-direction")))
     {
       Direction hd = to_dir (me->get_property ("head-direction"));
 
       return (hd == d)
	 ? unsmob_grob (me->get_object ("note-head"))
	 : 0;
     }
  
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
	return (int) rint (Staff_symbol_referencer::get_position (h));
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
      Grob *one_head = head (me, d);
      if (!one_head && dynamic_cast<Spanner*> (me)) 
	one_head = Tie::head (dynamic_cast<Spanner*> (me)->broken_neighbor (d), d);
      
      Grob *stem = one_head ? Rhythmic_head::get_stem (one_head) : 0;
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
  else if (int p = get_position (me))
    return Direction (sign (p));
  
  return to_dir (me->get_property("neutral-direction"));
}


MAKE_SCHEME_CALLBACK (Tie, calc_direction, 1);
SCM
Tie::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *yparent = me->get_parent (Y_AXIS);
  if ((Tie_column::has_interface (yparent)
       || Semi_tie_column::has_interface (yparent)) 
      && unsmob_grob_array (yparent->get_object ("ties"))
      //      && unsmob_grob_array (yparent->get_object ("ties"))->size () > 1
      )
    {
      /* trigger positioning. */
      (void) yparent->get_property ("positioning-done");

      return me->get_property_data ("direction");
    }
  else
    return scm_from_int (Tie::get_default_dir (me));
}


SCM
Tie::get_default_control_points (Grob *me_grob)
{
  Spanner *me = dynamic_cast<Spanner*> (me_grob);
  Grob *common  = me;
  common = me->get_bound (LEFT)->common_refpoint (common, X_AXIS); 
  common = me->get_bound (RIGHT)->common_refpoint (common, X_AXIS); 
  
  Tie_formatting_problem problem;
  problem.from_tie (me);
  
  Tie_specification spec = problem.get_tie_specification (0);
  if (!me->is_live ())
    return SCM_EOL;

  
  Ties_configuration conf
    = problem.generate_optimal_configuration ();
  
  return get_control_points (me, problem.common_x_refpoint (),
			     conf[0], problem.details_);
}

SCM
Tie::get_control_points (Grob *me,
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
  return controls;
}

MAKE_SCHEME_CALLBACK (Tie, calc_control_points, 1);
SCM
Tie::calc_control_points (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Grob *yparent = me->get_parent (Y_AXIS);
  if ((Tie_column::has_interface (yparent)
       || Semi_tie_column::has_interface (yparent)) 
      && unsmob_grob_array (yparent->get_object ("ties")))
    {
      extract_grob_set (yparent, "ties", ties);
      if (me->original() && ties.size() == 1
	  && !to_dir (me->get_property_data ("direction")))
	{
	  assert (ties[0] == me);
	  set_grob_direction (me, Tie::get_default_dir (me));
	    
	}      
      /* trigger positioning. */
      (void) yparent->get_property ("positioning-done");
    }

  SCM cp = me->get_property_data ("control-points");
  if (!scm_is_pair (cp))
    {
      cp = get_default_control_points (me);
    }

  return cp;
}

/*
  TODO: merge with Slur::print.
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
  for (SCM s = cp; scm_is_pair (s); s = scm_cdr (s))
    {
      b.control_[i] = ly_scm2offset (scm_car (s));
      i++;
    }

  Stencil a;

  SCM dash_definition = me->get_property ("dash-definition");
//  SCM p = me->get_property ("dash-period");
//  SCM f = me->get_property ("dash-fraction");
//  SCM interval = me->get_property ("dash-interval");
//  if (scm_is_number (p) && scm_is_number (f))
//    a = Lookup::dashed_slur (b,
//			     line_thick,
//			     robust_scm2double (p, 1.0),
//			     robust_scm2double (f, 0));
//  else
    a = Lookup::slur (b,
		      get_grob_direction (me) * base_thick,
		      line_thick,
                      dash_definition);
//                      robust_scm2double (p, 1.0),
//                      robust_scm2double (f, 0),
//                      robust_scm2double (interval, 1.0));

#if DEBUG_TIE_SCORING
  SCM annotation = me->get_property ("annotation");
  if (!scm_is_string (annotation))
    {
      SCM debug = me->layout ()->lookup_variable (ly_symbol2scm ("debug-tie-scoring"));
      if (to_boolean (debug))
	annotation = me->get_property ("quant-score");
    }
  if (scm_is_string (annotation))
    {
      string str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      Stencil tm = *unsmob_stencil (Text_interface::interpret_markup
				    (me->layout ()->self_scm (), properties,
				     annotation));
      tm.translate (Offset (b.control_[3][X_AXIS] + 0.5,
			    b.control_[0][Y_AXIS] * 2));
      tm = tm.in_color (1, 0, 0);

      /*
	It would be nice if we could put this in a different layer,
	but alas, this must be done with a Tie override.
       */
      a.add_stencil (tm);
    }
#endif

  return a.smobbed_copy ();
}

ADD_INTERFACE (Tie,
	       "A horizontal curve connecting two noteheads.",
	       
	       /* properties */
	       "annotation "
	       "avoid-slur " 	//  UGH.
	       "control-points "
	       "dash-fraction "
	       "dash-period "
	       "details "
	       "direction "
	       "head-direction "
	       "line-thickness "
	       "neutral-direction "
	       "quant-score "
	       "separation-item "
	       "staff-position "
	       "thickness "
	       );





