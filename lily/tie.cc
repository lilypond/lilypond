/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "tie.hh"

#include <math.h>

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

/*
  tie: Connect two noteheads.

  What if we have

  c4 ~ \clef bass ; c4 or

  c4 \staffchange c4

  do we have non-horizontal ties then?
*/

void
Tie::set_head (Grob *me, Direction d, Grob *h)
{
  assert (!head (me, d));
  index_set_cell (me->get_property ("head-pair"), d, h->self_scm ());

  dynamic_cast<Spanner *> (me)->set_bound (d, h);
  me->add_dependency (h);
}

void
Tie::set_interface (Grob *me)
{
  me->set_property ("head-pair", scm_cons (SCM_EOL, SCM_EOL));
}

Grob *
Tie::head (Grob *me, Direction d)
{
  SCM c = me->get_property ("head-pair");

  if (scm_is_pair (c))
    return unsmob_grob (index_get_cell (c, d));
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

Real
Tie::get_position (Grob *me)
{
  Direction d = head (me, LEFT) ? LEFT : RIGHT;
  return Staff_symbol_referencer::get_position (head (me, d));
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
  Item *sl = head (me, LEFT) ? Rhythmic_head::get_stem (head (me, LEFT)) : 0;
  Item *sr = head (me, RIGHT) ? Rhythmic_head::get_stem (head (me, RIGHT)) : 0;
  if (sl && sr)
    {
      if (get_grob_direction (sl) == UP
	  && get_grob_direction (sr) == UP)
	return DOWN;
    }
  else if (sl || sr)
    {
      Item *s = sl ? sl : sr;
      return -get_grob_direction (s);
    }

  return UP;
}

void
Tie::set_direction (Grob *me)
{
  if (!get_grob_direction (me))
    {
      if (Tie_column::has_interface (me->get_parent (Y_AXIS)))
	Tie_column::set_directions (me->get_parent (Y_AXIS));
      else
	set_grob_direction (me, Tie::get_default_dir (me));
    }
}

MAKE_SCHEME_CALLBACK (Tie, print, 1);
SCM
Tie::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM cp = me->get_property ("control-points");
  if (!scm_is_pair (cp))		// list is more accurate
    {
      cp = get_control_points (smob);
      me->set_property ("control-points", cp);
    }

  if (!scm_is_pair (cp))
    return Stencil ().smobbed_copy ();

  Real staff_thick = Staff_symbol_referencer::line_thickness (me);
  Real base_thick = robust_scm2double (me->get_property ("thickness"), 1);
  Real thick = base_thick * staff_thick;

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
			     thick,
			     robust_scm2double (p, 1.0),
			     robust_scm2double (f, 0));
  else
    a = Lookup::slur (b,
		      get_grob_direction (me) * staff_thick,
		      thick);

  return a.smobbed_copy ();
}

ADD_INTERFACE (Tie,
	       "tie-interface",
	       "A tie connecting two noteheads.\n",

	       "y-offset dash-period dash-fraction "
	       "staffline-clearance control-points head-pair "
	       "details thickness x-gap direction minimum-length");
