/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>		// ceil

#include "axis-group-interface.hh"
#include "note-column.hh"
#include "stem.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "rest.hh"
#include "note-head.hh"

bool
Note_column::rest_b (Grob*me) 
{
  return unsmob_grob (me->get_grob_property ("rest"));
}

int
Note_column::shift_compare (Grob *const &p1, Grob *const&p2)
{
  SCM s1 = p1->get_grob_property ("horizontal-shift");
  SCM s2 = p2->get_grob_property ("horizontal-shift");

  int h1 = (gh_number_p (s1))?  gh_scm2int (s1) :0;
  int h2 = (gh_number_p (s2)) ? gh_scm2int (s2):0;
  return h1 - h2;
}

void
Note_column::set_interface (Grob* me)
{
  me->set_grob_property ("note-heads", SCM_EOL);  
  me->set_interface (ly_symbol2scm ("note-column-interface"));
  
  Axis_group_interface::set_interface (me);
  Axis_group_interface::set_axes (me, X_AXIS, Y_AXIS);
}

Item *
Note_column::stem_l (Grob*me) 
{
  SCM s = me->get_grob_property ("stem");
  return  dynamic_cast<Item*>(unsmob_grob (s));
}
  
Slice
Note_column::head_positions_interval(Grob *me)
{
  Slice  iv;

  iv.set_empty ();

  SCM h = me->get_grob_property ("note-heads");
  for (; gh_pair_p (h); h = gh_cdr (h))
    {
      Grob *se = unsmob_grob (gh_car (h));
      
      int j = int (Staff_symbol_referencer::position_f (se));
      iv.unite (Slice (j,j));
    }
  return iv;
}

Direction
Note_column::dir (Grob*  me)
{
  Grob *stem = unsmob_grob (me->get_grob_property ("stem"));
  if (stem && Stem::has_interface (stem))
    return Stem::get_direction (stem);
  else if (gh_pair_p (me->get_grob_property ("note-heads")))
    return (Direction)sign (head_positions_interval (me).center ());

  programming_error ("Note column without heads and stem!");
  return CENTER;
}


void
Note_column::set_stem (Grob*me,Grob * stem_l)
{
  me->set_grob_property ("stem", stem_l->self_scm ());
  me->add_dependency (stem_l);
  Axis_group_interface::add_element (me, stem_l);
}

void
Note_column::add_head (Grob*me,Grob *h)
{
  if (Rest::has_interface (h))
    {
      me->set_grob_property ("rest", h->self_scm ());
    }
  else if (Note_head::has_interface (h))
    {
      Pointer_group_interface::add_element (me, "note-heads",h);
    }
  Axis_group_interface::add_element (me, h);
}

/**
  translate the rest symbols vertically by amount DY_I.
 */
void
Note_column::translate_rests (Grob*me,int dy_i)
{
  Grob * r = unsmob_grob (me->get_grob_property ("rest"));
  if (r)
    {
      r->translate_axis (dy_i * Staff_symbol_referencer::staff_space (r)/2.0, Y_AXIS);
    }
}


void
Note_column::set_dotcol (Grob*me,Grob *d)
{
  Axis_group_interface::add_element (me, d);
}




Grob*
Note_column::first_head (Grob*me) 
{
  Grob * st = stem_l (me);
  return st?  Stem::first_head (st): 0; 
}

bool
Note_column::has_interface (Grob*me)
{
  return me && me->has_interface (ly_symbol2scm ("note-column-interface"));
}
