/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "note-column.hh"

#include <cmath>		// ceil

#include "axis-group-interface.hh"
#include "stem.hh"
#include "warn.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "rest.hh"
#include "note-head.hh"
#include "accidental-placement.hh"

/*
  TODO: figure out if we can prune this class. This is just an
  annoying layer between (rest)collision & (note-head + stem)
 */

bool
Note_column::has_rests (Grob*me) 
{
  return unsmob_grob (me->get_property ("rest"));
}

int
Note_column::shift_compare (Grob *const &p1, Grob *const&p2)
{
  SCM s1 = p1->get_property ("horizontal-shift");
  SCM s2 = p2->get_property ("horizontal-shift");

  int h1 = (scm_is_number (s1))?  scm_to_int (s1) :0;
  int h2 = (scm_is_number (s2)) ? scm_to_int (s2):0;
  return h1 - h2;
}

Item *
Note_column::get_stem (Grob*me) 
{
  SCM s = me->get_property ("stem");
  return  unsmob_item (s);
}
  
Slice
Note_column::head_positions_interval (Grob *me)
{
  Slice  iv;

  iv.set_empty ();

  SCM h = me->get_property ("note-heads");
  for (; scm_is_pair (h); h = scm_cdr (h))
    {
      Grob *se = unsmob_grob (scm_car (h));
      
      int j = Staff_symbol_referencer::get_rounded_position (se);
      iv.unite (Slice (j,j));
    }
  return iv;
}

Direction
Note_column::dir (Grob*  me)
{
  Grob *stem = unsmob_grob (me->get_property ("stem"));
  if (stem && Stem::has_interface (stem))
    return Stem::get_direction (stem);
  else if (scm_is_pair (me->get_property ("note-heads")))
    return (Direction)sign (head_positions_interval (me).center ());

  programming_error ("Note column without heads and stem!");
  return CENTER;
}


void
Note_column::set_stem (Grob*me,Grob * stem)
{
  me->set_property ("stem", stem->self_scm ());
  me->add_dependency (stem);
  Axis_group_interface::add_element (me, stem);
}


Grob*
Note_column::get_rest (Grob*me)
{
  return unsmob_grob (me->get_property ("rest"));
}
  
void
Note_column::add_head (Grob*me,Grob *h)
{
  bool both = false;
  if (Rest::has_interface (h))
    {
      if (scm_is_pair (me->get_property ("note-heads")))
	both = true;
      else
	me->set_property ("rest", h->self_scm ());
    }
  else if (Note_head::has_interface (h))
    {
      if (unsmob_grob (me->get_property ("rest")))
	both = true;
      Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-heads"),h);
    }

  if (both)
    me->warning ("Can't have rests and note heads together on a stem.");
  else
    Axis_group_interface::add_element (me, h);
}

/**
  translate the rest symbols vertically by amount DY, but only if
  they have no staff-position set.
*/
void
Note_column::translate_rests (Grob*me, int dy)
{
  Grob * r = unsmob_grob (me->get_property ("rest"));
  if (r && !scm_is_number (r->get_property ("staff-position")))
    {
      r->translate_axis (dy * Staff_symbol_referencer::staff_space (r)/2.0, Y_AXIS);
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
  SCM heads = me->get_property ("note-heads");
  Grob * acc = 0;
  for (;scm_is_pair (heads); heads =scm_cdr (heads))
    {
      Grob * h = unsmob_grob (scm_car (heads));
      acc = h ? unsmob_grob (h->get_property ("accidental-grob")) : 0;
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

