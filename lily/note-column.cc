/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>		// ceil


#include "dot-column.hh"
#include "note-column.hh"
#include "beam.hh"
#include "note-head.hh"
#include "stem.hh"
#include "rest.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"

bool
Note_column::rest_b () const
{
  SCM r = get_elt_property ("rests");

  return gh_pair_p (r);
}

int
Note_column::shift_compare (Note_column *const &p1, Note_column*const&p2)
{
  SCM s1 = p1->get_elt_property ("horizontal-shift");
  SCM s2 = p2->get_elt_property ("horizontal-shift");

  int h1 = (gh_number_p (s1))?  gh_scm2int (s1) :0;
  int h2 = (gh_number_p (s2)) ? gh_scm2int (s2):0;
  return h1 - h2;
}

Note_column::Note_column()
{
  set_elt_property ("rests", SCM_EOL);
  set_elt_property ("note-heads", SCM_EOL);  
  set_axes (X_AXIS, Y_AXIS);
}

Stem *
Note_column::stem_l () const
{
  SCM s = get_elt_property ("stem");
  return dynamic_cast<Stem*> (unsmob_element (s));

}

  
Slice
Note_column::head_positions_interval() const
{
  Slice  iv;

  iv.set_empty ();

  SCM h = get_elt_property ("note-heads");
  for (; gh_pair_p (h); h = gh_cdr (h))
    {
      Score_element *se = unsmob_element (gh_car (h));
      Staff_symbol_referencer_interface si (se); 
      
      int j = int (si.position_f ());
      iv.unite (Slice (j,j));
    }
  return iv;
}

Direction
Note_column::dir () const
{
  if (stem_l ())
    return stem_l ()->get_direction ();
  else if (gh_pair_p (get_elt_property ("note-heads")))
    return (Direction)sign (head_positions_interval().center ());

  programming_error ("Note column without heads and stem!");
  return CENTER;
}


void
Note_column::set_stem (Stem * stem_l)
{
  set_elt_property ("stem", stem_l->self_scm_);

  add_dependency (stem_l);
  add_element (stem_l);
}



void
Note_column::add_head (Rhythmic_head *h)
{
  if (Rest*r=dynamic_cast<Rest *> (h))
    {
      Group_interface gi (this, "rests");
      gi.add_element (h);
    }
  if (Note_head *nh=dynamic_cast<Note_head *> (h))
    {
      Group_interface gi (this, "note-heads");
      gi.add_element (nh);
    }
  add_element (h);
}

/**
  translate the rest symbols vertically by amount DY_I.
 */
void
Note_column::translate_rests (int dy_i)
{
  SCM s = get_elt_property ("rests");
  for (; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * se = unsmob_element (gh_car (s));
      Staff_symbol_referencer_interface si (se);

      se->translate_axis (dy_i * si.staff_space ()/2.0, Y_AXIS);
    }
}


void
Note_column::set_dotcol (Dot_column *d)
{
  add_element (d);
}

/*
  [TODO]
  handle rest under beam (do_post: beams are calculated now)
  what about combination of collisions and rest under beam.

  Should lookup
    
    rest -> stem -> beam -> interpolate_y_position ()
*/

void
Note_column::do_post_processing ()
{
  if (!stem_l () || !rest_b ())
    return;

  Beam * b = stem_l ()->beam_l ();
  if (!b || !b->stem_count ())
    return;
  
  /* ugh. Should be done by beam.
     (what? should be done --jcn)
    scary too?: height is calculated during post_processing
   */
  Real beam_dy = 0;
  Real beam_y = 0;

  SCM s = b->get_elt_property ("height");
  if (gh_number_p (s))
    beam_dy = gh_scm2double (s);
  
  s = b->get_elt_property ("y-position");
  if (gh_number_p (s))
    beam_y = gh_scm2double (s);

  Real x0 = b->first_visible_stem ()->hpos_f ();
  Real dx = b->last_visible_stem ()->hpos_f () - x0;
  Real dydx = beam_dy && dx ? beam_dy/dx : 0;

  Direction d = stem_l ()->get_direction ();
  Real beamy = (stem_l ()->hpos_f () - x0) * dydx + beam_y;

  s = get_elt_property ("rests");
  Score_element * se = unsmob_element (gh_car (s));
  Staff_symbol_referencer_interface si (se);

  Real staff_space = si.staff_space ();      
  Real rest_dim = extent (Y_AXIS)[d]*2.0  /staff_space ;

  Real minimum_dist
    = paper_l ()->get_var ("restcollision_minimum_beamdist") ;
  Real dist =
    minimum_dist +  -d  * (beamy - rest_dim) >? 0;

  int stafflines = si.line_count ();

  // move discretely by half spaces.
  int discrete_dist = int (ceil (dist ));

  // move by whole spaces inside the staff.
  if (discrete_dist < stafflines+1)
    discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);

  translate_rests (-d *  discrete_dist);
}


Interval
Note_column::rest_dim () const
{
  Interval restdim;
  SCM s = get_elt_property ("rests");
  for (; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * sc = unsmob_element ( gh_car (s));
      restdim.unite (sc->extent (Y_AXIS));
    }
  
  return restdim;
}

Note_head*
Note_column::first_head () const
{
  Stem * st = stem_l ();
  return st?  st->first_head (): 0; 
}
