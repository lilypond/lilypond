/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "script-interface.hh"

#include "directional-element-interface.hh"
#include "item.hh"
#include "warn.hh"
#include "font-interface.hh"
#include "side-position-interface.hh"
#include "output-def.hh"
#include "lookup.hh"
#include "stem.hh"
#include "note-column.hh"

Stencil
Script_interface::get_stencil (Grob *me, Direction d)
{
  SCM s = me->get_property ("script-stencil");
  assert (scm_is_pair (s));

  SCM key = scm_car (s);
  if (key == ly_symbol2scm ("feta"))
    {
      SCM name_entry = scm_cdr (s);
      SCM str = ((scm_is_pair (name_entry)) ? index_get_cell (name_entry, d)
		 : name_entry);
      return Font_interface::get_default_font (me)
	->find_by_name ("scripts." + ly_scm2string (str));
    }
  else
    assert (false);

  return Stencil ();
}

MAKE_SCHEME_CALLBACK (Script_interface, calc_positioning_done, 1);
SCM
Script_interface::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (Grob *par = me->get_parent (X_AXIS))
    {
      Grob *stem = Note_column::get_stem (par);
      if (stem && Stem::first_head (stem))
	me->set_parent (Stem::first_head (stem), X_AXIS);
    }
  return SCM_BOOL_T;
}

Direction
Script_interface::get_direction (Grob *me)
{
  Direction relative_dir = Direction (1);
  SCM reldir = me->get_property ("side-relative-direction");
  if (is_direction (reldir))
    relative_dir = to_dir (reldir);

  SCM other_elt = me->get_object ("direction-source");
  Grob *e = unsmob_grob (other_elt);
  if (e)
    return (Direction) (relative_dir * get_grob_direction (e));

  return CENTER;
}

MAKE_SCHEME_CALLBACK (Script_interface, calc_direction, 1);
SCM
Script_interface::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Direction d = Script_interface::get_direction (me);

  if (!d)
    {
      me->programming_error ("script direction not yet known");
      d = DOWN;
    }

  (void) me->get_property ("positioning-done");
  return scm_from_int (d);
}

MAKE_SCHEME_CALLBACK (Script_interface, calc_cross_staff, 1);
SCM
Script_interface::calc_cross_staff (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *stem = Note_column::get_stem (me->get_parent (X_AXIS));

  if (stem && to_boolean (stem->get_property ("cross-staff")))
    return SCM_BOOL_T;

  Grob *slur = unsmob_grob (me->get_object ("slur"));
  SCM avoid_slur = me->get_property ("avoid-slur");
  if (slur && to_boolean (slur->get_property ("cross-staff"))
      && (avoid_slur == ly_symbol2scm ("outside")
	  || avoid_slur == ly_symbol2scm ("around")))
    return SCM_BOOL_T;

  return SCM_BOOL_F;
}

MAKE_SCHEME_CALLBACK (Script_interface, print, 1);

SCM
Script_interface::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Direction dir = get_grob_direction (me);

  return get_stencil (me, dir).smobbed_copy ();
}

struct Text_script
{
  DECLARE_GROB_INTERFACE ();
};

ADD_INTERFACE (Text_script,
	       "An object that is put above or below a note.",

	       /* properties */
	       "add-stem-support "
	       "avoid-slur "
	       "script-priority "
	       "slur "
	       );

/*
  Hmm. Where should we put add-stem-support ?
*/
ADD_INTERFACE (Script_interface,
	       "An object that is put above or below a note.",

	       /* properties */
	       "add-stem-support "
	       "avoid-slur "
	       "direction-source "
	       "positioning-done "
	       "script-priority "
	       "script-stencil "
	       "side-relative-direction "
	       "slur "
	       "slur-padding "
	       "toward-stem-shift "
	       );

