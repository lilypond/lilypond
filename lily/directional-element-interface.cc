/*
  directional-element-interface.cc -- implement Directional_element

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "directional-element-interface.hh"
#include "warn.hh"
#include "grob.hh"

Direction
get_grob_direction (Grob *me)
{
  SCM d = me->get_property ("direction");
  if (d == ly_symbol2scm ("calculation-in-progress"))
    {
      programming_error ("Grob direction requested while calculation in progress. ");
      return UP;
    }
  if (!is_direction (d))
    return CENTER;

  return to_dir (d);
}

void
set_grob_direction (Grob *me, Direction d)
{
  SCM sd = scm_from_int (d);
  me->set_property ("direction", sd);
}
