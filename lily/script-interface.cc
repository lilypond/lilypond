/*
  script-interface.cc -- implement Script_interface

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  else if (key == ly_symbol2scm ("accordion"))
    return Lookup::accordion (scm_cdr (s), 1.0,
			      Font_interface::get_default_font (me));
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

MAKE_SCHEME_CALLBACK (Script_interface, calc_direction, 1);
SCM
Script_interface::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Direction d = Side_position_interface::get_direction (me);

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
  return scm_from_bool (stem && to_boolean (stem->get_property ("cross-staff")));
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
	       "An object that is put above or below a note",

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
	       "An object that is put above or below a note",
	       "add-stem-support "
	       "avoid-slur "
	       "positioning-done "
	       "script-priority "
	       "script-stencil "
	       "slur "
	       "slur-padding "
	       );

