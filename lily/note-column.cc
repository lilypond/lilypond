/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>		// ceil

#include "axis-group-interface.hh"
#include "note-column.hh"
#include "stem.hh"
#include "warn.hh"
#include "paper-def.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "rest.hh"
#include "note-head.hh"
#include "accidental-placement.hh"

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

Item *
Note_column::get_stem (Grob*me) 
{
  SCM s = me->get_grob_property ("stem");
  return  unsmob_item (s);
}
  
Slice
Note_column::head_positions_interval (Grob *me)
{
  Slice  iv;

  iv.set_empty ();

  SCM h = me->get_grob_property ("note-heads");
  for (; gh_pair_p (h); h = ly_cdr (h))
    {
      Grob *se = unsmob_grob (ly_car (h));
      
      int j = int (Staff_symbol_referencer::get_position (se));
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
Note_column::set_stem (Grob*me,Grob * stem)
{
  me->set_grob_property ("stem", stem->self_scm ());
  me->add_dependency (stem);
  Axis_group_interface::add_element (me, stem);
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
      Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-heads"),h);
    }
  Axis_group_interface::add_element (me, h);
}

/**
  translate the rest symbols vertically by amount DY_I, but only if
  they have no staff-position set.
*/
void
Note_column::translate_rests (Grob*me,int dy_i)
{
  Grob * r = unsmob_grob (me->get_grob_property ("rest"));
  if (r && !gh_number_p (r->get_grob_property ("staff-position")))
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
  Grob * st = get_stem (me);
  return st?  Stem::first_head (st): 0; 
}


/*
  Return the first Accidentals grob that we find in a note-head. 
 */
Grob* 
Note_column::accidentals (Grob *me)
{
  SCM heads = me->get_grob_property ("note-heads");
  Grob * acc = 0;
  for (;gh_pair_p (heads); heads =gh_cdr (heads))
    {
      Grob * h = unsmob_grob (gh_car (heads));
      acc = h ? unsmob_grob (h->get_grob_property ("accidental-grob")) : 0;
      if (acc)
	break;
    }

  if (!acc)
    return 0;
  
  if (Accidental_placement::has_interface (acc->get_parent (X_AXIS)))
    return acc->get_parent (X_AXIS);

  /* compatibility. */
  return  acc;
}



ADD_INTERFACE (Note_column,"note-column-interface",
  "Stem and noteheads combined",
  "arpeggio note-heads rest-collision rest horizontal-shift stem accidentals force-hshift");

