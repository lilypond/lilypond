/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>		// ceil

#include "axis-group-interface.hh"
#include "note-column.hh"
#include "stem.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"

bool
Note_column::rest_b () const
{
  return unsmob_element (get_elt_pointer ("rest"));
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

Note_column::Note_column( SCM s)
  : Item (s)
{
  set_elt_pointer ("note-heads", SCM_EOL);  
  Axis_group_interface (this).set_interface ();
  Axis_group_interface (this).set_axes (X_AXIS, Y_AXIS);
  Group_interface (this, "interfaces").add_thing (ly_symbol2scm ("Note_column"));
}

Stem *
Note_column::stem_l () const
{
  SCM s = get_elt_pointer ("stem");
  return dynamic_cast<Stem*> (unsmob_element (s));
}

  
Slice
Note_column::head_positions_interval(Score_element *me)
{
  Slice  iv;

  iv.set_empty ();

  SCM h = me->get_elt_pointer ("note-heads");
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
Note_column::static_dir (Score_element*  me)
{
  Score_element *stem = unsmob_element (me->get_elt_pointer ("stem"));
  if (dynamic_cast<Stem*> (stem))
    return dynamic_cast<Stem*> (stem)->get_direction ();
  else if (gh_pair_p (me->get_elt_pointer ("note-heads")))
    return (Direction)sign (head_positions_interval (me).center ());

  programming_error ("Note column without heads and stem!");
  return CENTER;
}


Direction
Note_column::dir () const
{
  return static_dir ((Score_element*) this);
}

void
Note_column::set_stem (Score_element * stem_l)
{
  set_elt_pointer ("stem", stem_l->self_scm_);

  add_dependency (stem_l);
  Axis_group_interface (this).add_element (stem_l);
}

void
Note_column::add_head (Score_element *h)
{
  if (to_boolean (h->get_elt_property ("rest-interface")))
    {
      this->set_elt_pointer ("rest", h->self_scm_);
    }
  else if (to_boolean (h->get_elt_property ("note-head-interface")))
    {
      Pointer_group_interface gi (this, "note-heads");
      gi.add_element (h);
    }
  Axis_group_interface (this).add_element (h);
}

/**
  translate the rest symbols vertically by amount DY_I.
 */
void
Note_column::translate_rests (int dy_i)
{
  Score_element * r = unsmob_element (get_elt_pointer ("rest"));
  if (r)
    {
      Staff_symbol_referencer_interface si (r);
      r->translate_axis (dy_i * si.staff_space ()/2.0, Y_AXIS);
    }
}


void
Note_column::set_dotcol (Score_element *d)
{
  Axis_group_interface (this).add_element (d);
}



Interval
Note_column::rest_dim () const
{
  Score_element * r = unsmob_element (get_elt_pointer ("rest"));
  return r->extent (Y_AXIS);
}

Rhythmic_head*
Note_column::first_head () const
{
  Stem * st = stem_l ();
  return st?  st->first_head (): 0; 
}
