/*
  script-interface.hh --

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SCRIPT_INTERFACE_HH
#define SCRIPT_INTERFACE_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

/**
   Articulation marks (and the like) that are attached to notes/stems.
   Needs support from Staff_side for proper operation.  Staff_side
   handles the positioning.
*/
class Script_interface
{
public:
  static Stencil get_stencil (Grob *, Direction d);
  DECLARE_GROB_INTERFACE();
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
};

void make_script_from_event (Grob *p,  Context *tg,
			     SCM type, int index);

#endif /* SCRIPT_INTERFACE_HH */

