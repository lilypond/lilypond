/*
  balloon.cc -- implement Balloon

  source file of the GNU LilyPond music typesetter

  (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "text-interface.hh"
#include "grob.hh"
#include "line-interface.hh"
#include "lookup.hh"
#include "font-interface.hh"
#include "lily-guile.hh"
#include "output-def.hh"
#include "misc.hh"

class Balloon_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_GROB_INTERFACE ();
};

MAKE_SCHEME_CALLBACK (Balloon_interface, print, 1);
SCM
Balloon_interface::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Grob *p = me->get_parent (X_AXIS);
  
  Offset off (me->relative_coordinate (p, X_AXIS),
	      me->relative_coordinate (p, Y_AXIS));

  Box b (p->extent (p, X_AXIS),
	 p->extent (p, Y_AXIS));

  Real padding = robust_scm2double (me->get_property ("padding"), .1);
  b.widen (padding, padding);

  // FIXME
  Stencil fr = Lookup::frame (b, 0.1, 0.05);

  SCM bt = me->get_property ("text");
  SCM chain = Font_interface::text_font_alist_chain (me);

  SCM stencil = Text_interface::interpret_markup (me->layout ()->self_scm (),
						  chain, bt);

  Stencil *text_stil = unsmob_stencil (stencil);

  Offset z1;
  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis a ((Axis)i);
      z1[a] = b[a].linear_combination (sign (off[a]));
      text_stil->align_to (a, -sign (off[a]));
    }

  Offset z2 = z1 + off;

  fr.add_stencil (Line_interface::line (me, z1, z2));

  text_stil->translate (z2);
  fr.add_stencil (*text_stil);

  fr.translate (-off);
  return fr.smobbed_copy ();
}

ADD_INTERFACE (Balloon_interface,
	       "A collection of routines to put text balloons around an"
	       " object.",

	       /* properties */
	       "padding "
	       "text "
	       );

