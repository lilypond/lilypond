/*   
  script-interface.cc --  implement Script_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "directional-element-interface.hh"
#include "warn.hh"
#include "script-interface.hh"
#include "font-interface.hh"
#include "side-position-interface.hh"
#include "output-def.hh"
#include "item.hh"
#include "stencil.hh"
#include "lookup.hh"
#include "stem.hh"
#include "note-column.hh"

Stencil
Script_interface::get_stencil (Grob *me, Direction d)
{
  SCM s = me->get_property ("script-stencil");
  assert (scm_is_pair (s));

  SCM key = ly_car (s);
  if (key == ly_symbol2scm ("feta"))
    {
      SCM name_entry = ly_cdr (s);
      SCM str = ((scm_is_pair (name_entry)) ? index_get_cell (name_entry, d)
		 : name_entry);
      return Font_interface::get_default_font (me)
	->find_by_name ("scripts-" + ly_scm2string (str));
    }
  else if (key == ly_symbol2scm ("accordion"))
    return Lookup::accordion (ly_cdr (s), 1.0,
			      Font_interface::get_default_font (me));
  else
    assert (false);

  return Stencil ();
}

MAKE_SCHEME_CALLBACK (Script_interface, before_line_breaking, 1);
SCM
Script_interface::before_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Direction d = Side_position_interface::get_direction (me);

  if (!d)
    {
      /* FIXME: This should never happen: `arbitrary' directions.  */
      programming_error ("Script direction not yet known!");
      d = DOWN;
    }

  set_grob_direction (me, d);

  if (Grob *par = me->get_parent (X_AXIS))
    {
      Grob *stem = Note_column::get_stem (par);
      if (stem && Stem::first_head (stem))
	me->set_parent (Stem::first_head (stem), X_AXIS);
    }
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Script_interface, print, 1);

SCM
Script_interface::print (SCM smob)
{
  Grob *me= unsmob_grob (smob);

  Direction dir = Side_position_interface::get_direction (me);
  if (!dir)
    {
      programming_error ("Script direction not known, but stencil wanted.");
      dir = DOWN;
    }
  return get_stencil (me, dir).smobbed_copy ();
}

struct Text_script
{
  static bool has_interface (Grob*);
};

ADD_INTERFACE (Text_script,"text-script-interface",
  "An object that is put above or below a note",
  "add-stem-support slur script-priority inside-slur");


/*
  Hmm. Where should we put add-stem-support ?
 */
ADD_INTERFACE (Script_interface, "script-interface",
  "An object that is put above or below a note",
  "add-stem-support slur-padding slur script-priority script-stencil inside-slur");

