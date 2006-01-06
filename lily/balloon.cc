/*
  balloon.cc -- implement Balloon

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  static bool has_interface (Grob *);
};

MAKE_SCHEME_CALLBACK (Balloon_interface, print, 1);
SCM
Balloon_interface::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM stil = me->get_property ("original-stencil");
  if (!unsmob_stencil (stil))
    return stil;

  SCM scm_off = me->get_property ("balloon-text-offset");

  if (!is_number_pair (scm_off))
    return stil;

  Offset off = ly_scm2offset (scm_off);
  Stencil *s = unsmob_stencil (stil);
  Box orig_extent = s->extent_box ();
  Box box_extent = orig_extent;

  Real w = robust_scm2double (me->get_property ("balloon-padding"), .1);
  box_extent.widen (w, w);

  // FIXME
  Stencil fr = Lookup::frame (box_extent, 0.1, 0.05);

  fr.add_stencil (*s);

  SCM bt = me->get_property ("balloon-text");
  SCM chain = Font_interface::text_font_alist_chain (me);
  chain = scm_cons (me->get_property ("balloon-text-props"), chain);

  SCM text = Text_interface::interpret_markup (me->layout ()->self_scm (),
					       chain, bt);

  Stencil *text_stil = unsmob_stencil (text);

  Offset z1;
  for (int i = X_AXIS; i < NO_AXES; i++)
    {
      Axis a ((Axis)i);
      z1[a] = box_extent [a].linear_combination (sign (off[a]));
      text_stil->align_to (a, -sign (off[a]));
    }

  Offset z2 = z1 + off;

  fr.add_stencil (Line_interface::line (me, z1, z2));

  text_stil->translate (z2);
  fr.add_stencil (*text_stil);

  fr = Stencil (orig_extent, fr.expr ());
  return fr.smobbed_copy ();
}

ADD_INTERFACE (Balloon_interface, "text-balloon-interface",
	       "A collection of routines to put text balloons around an object.",

	       /* properties */
	       "balloon-padding "
	       "balloon-text-props "
	       "balloon-text-offset "
	       "balloon-text "
	       "original-stencil ");

