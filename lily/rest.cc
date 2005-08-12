/*
  rest.cc -- implement Rest

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "rest.hh"

#include "stencil.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "dots.hh"
#include "paper-score.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"

// -> offset callback
MAKE_SCHEME_CALLBACK (Rest, after_line_breaking, 1);
SCM
Rest::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  int bt = scm_to_int (me->get_property ("duration-log"));
  int lc = Staff_symbol_referencer::line_count (me);
  Real ss = Staff_symbol_referencer::staff_space (me);
  if (lc % 2)
    {
      if (bt == 0 && lc > 1)
	me->translate_axis (ss, Y_AXIS);
    }
  else
    {
      me->translate_axis (ss / 2, Y_AXIS);
    }

  Grob *d = unsmob_grob (me->get_object ("dot"));
  if (d && bt > 4) // UGH.
    {
      d->set_property ("staff-position",
		       scm_from_int ((bt == 7) ? 4 : 3));
    }
  if (d && bt >= -1 && bt <= 1) // UGH again.
    {
      d->set_property ("staff-position",
		       scm_from_int ((bt == 0) ? -1 : 1));
    }
  return SCM_UNSPECIFIED;
}

/*
  make this function easily usable in C++
*/
String
Rest::glyph_name (Grob *me, int balltype, String style, bool try_ledgers)
{
  bool ledgered_b = false;
  if (try_ledgers && (balltype == 0 || balltype == 1))
    {
      Real rad = Staff_symbol_referencer::staff_radius (me) * 2.0;
      Real pos = Staff_symbol_referencer::get_position (me);

      /*
	Figure out when the rest is far enough outside the staff. This
	could bemore generic, but hey, we understand this even after
	dinner.
      */
      ledgered_b |= (balltype == 0) && (pos >= +rad + 2 || pos < -rad);
      ledgered_b |= (balltype == 1) && (pos <= -rad - 2 || pos > +rad);
    }

  String actual_style (style.to_str0 ());

  if ((style == "mensural") || (style == "neomensural"))
    {

      /*
	FIXME: Currently, ancient font does not provide ledgered rests;
	hence the "o" suffix in the glyph name is bogus.  But do we need
	ledgered rests at all now that we can draw ledger lines with
	variable width, length and blotdiameter? -- jr
      */
      ledgered_b = 0;

      /*
	There are no 32th/64th/128th mensural/neomensural rests.  In
	these cases, revert back to default style.
      */
      if (balltype > 4)
	actual_style = "";
    }

  if ((style == "classical") && (balltype != 2))
    {
      /*
	classical style: revert back to default style for any rest other
	than quarter rest
      */
      actual_style = "";
    }

  if (style == "default")
    {
      /*
	Some parts of lily still prefer style "default" over "".
	Correct this here. -- jr
      */
      actual_style = "";
    }

  return ("rests." + to_string (balltype) + (ledgered_b ? "o" : "")
	  + actual_style);
}

MAKE_SCHEME_CALLBACK (Rest, print, 1);

SCM
Rest::brew_internal_stencil (SCM smob, bool ledgered)
{
  Grob *me = unsmob_grob (smob);

  SCM balltype_scm = me->get_property ("duration-log");
  if (!scm_is_number (balltype_scm))
    return Stencil ().smobbed_copy ();

  int balltype = scm_to_int (balltype_scm);

  String style;
  SCM style_scm = me->get_property ("style");
  if (scm_is_symbol (style_scm))
    style = ly_scm2string (scm_symbol_to_string (style_scm));

  Font_metric *fm = Font_interface::get_default_font (me);
  String font_char = glyph_name (me, balltype, style, ledgered);
  Stencil out = fm->find_by_name (font_char);
  if (out.is_empty ())
    me->warning (_f ("rest `%s' not found", font_char.to_str0 ()));

  return out.smobbed_copy ();
}

SCM
Rest::print (SCM smob)
{
  return brew_internal_stencil (smob, true);
}

MAKE_SCHEME_CALLBACK (Rest, extent_callback, 2);
/*
  We need the callback. The real stencil has ledgers depending on
  Y-position. The Y-position is known only after line breaking.  */
SCM
Rest::extent_callback (SCM smob, SCM ax)
{
  Axis a = (Axis) scm_to_int (ax);

  /*
    Don't want ledgers: ledgers depend on Y position, which depends on
    rest collision, which depends on stem size which depends on beam
    slop of opposite note column.

    consequence: we get too small extents and potential collisions
    with ledgered rests.
  */
  SCM m = brew_internal_stencil (smob, a != X_AXIS);
  return ly_interval2scm (unsmob_stencil (m)->extent (a));
}

MAKE_SCHEME_CALLBACK (Rest, polyphonic_offset_callback, 2);
SCM
Rest::polyphonic_offset_callback (SCM smob, SCM)
{
  Grob *me = unsmob_grob (smob);
  if (scm_is_number (me->get_property ("staff-position")))
    return scm_from_double (0);

  Direction d = get_grob_direction (me);
  Real off = 2 * d;
  if (off)
    off *= Staff_symbol_referencer::staff_space (me);

  return scm_from_double (off);
}

ADD_INTERFACE (Rest, "rest-interface",
	       "A rest symbol.",
	       "style direction minimum-distance");

