/*
 rest.cc -- implement Rest

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "rest.hh"
#include "dots.hh"
#include "paper-score.hh"
#include "staff-symbol-referencer.hh"

// -> offset callback
MAKE_SCHEME_CALLBACK (Rest,after_line_breaking,1);
SCM
Rest::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  int bt = gh_scm2int (me->get_grob_property ("duration-log"));
  int lc = Staff_symbol_referencer::line_count (me);
  Real ss = Staff_symbol_referencer::staff_space (me);
  if(lc % 2)
    {
      if (bt == 0 && lc > 1)
	{
	  me->translate_axis (ss , Y_AXIS);
	}
    }
  else
    {
      me->translate_axis (ss/2 , Y_AXIS);
    }

  Grob * d = unsmob_grob (me->get_grob_property ("dot"));
  if (d && bt > 4) // UGH.
    {
      d->set_grob_property ("staff-position",
			    gh_int2scm ((bt == 7) ? 4 : 3));
    }

  return SCM_UNSPECIFIED;
}

/*
  make this function easily usable in C++
 */
String
Rest::glyph_name (Grob *me, int balltype, String style)
{
  bool ledgered_b = false;

  if (balltype == 0 || balltype == 1)
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

  if ((style == "mensural") || (style == "neo_mensural")) {

    /*
      FIXME: Currently, ancient font does not provide ledgered rests;
      hence the "o" suffix in the glyph name is bogus.  But do we need
      ledgered rests at all now that we can draw ledger lines with
      variable width, length and blotdiameter? -- jr
    */
    ledgered_b = 0;

    /*
      There are no 32th/64th/128th mensural/neo_mensural rests.  In
      these cases, revert back to default style.
    */
    if (balltype > 4)
      actual_style = "";
  }

  if ((style == "classical") && (balltype != 2)) {
    /*
      classical style: revert back to default style for any rest other
      than quarter rest
    */
    actual_style = "";
  }

  if (style == "default") {
    /*
      Some parts of lily still prefer style "default" over "".
      Correct this here. -- jr
    */
    actual_style = "";
  }

  return ("rests-") + to_string (balltype) + (ledgered_b ? "o" : "") + actual_style;
}


MAKE_SCHEME_CALLBACK (Rest,brew_molecule,1);

SCM
Rest::brew_internal_molecule (SCM smob)
{
  Grob* me = unsmob_grob (smob);

  SCM balltype_scm = me->get_grob_property ("duration-log");
  if (!gh_number_p (balltype_scm))
    return Molecule ().smobbed_copy ();

  int balltype = gh_scm2int (balltype_scm);
  
  String style; 
  SCM style_scm = me->get_grob_property ("style");
  if (gh_symbol_p (style_scm))
    {
      style = ly_scm2string (scm_symbol_to_string (style_scm));
    }

  Font_metric *fm = Font_interface::get_default_font (me);
  String font_char = glyph_name (me, balltype, style);
  Molecule out = fm->find_by_name (font_char);
  if (out.empty_b())
    {
      me->warning (_f ("rest `%s' not found, ", font_char.to_str0 ()));
    }

  return out.smobbed_copy();
}

SCM 
Rest::brew_molecule (SCM smob) 
{
  return brew_internal_molecule (smob);
}
MAKE_SCHEME_CALLBACK (Rest,extent_callback,2);
/*
  We need the callback. The real molecule has ledgers depending on
  Y-position. The Y-position is known only after line breaking.  */
SCM
Rest::extent_callback (SCM smob, SCM ax)
{
  Axis a = (Axis) gh_scm2int (ax);
  SCM m = brew_internal_molecule (smob);
  return ly_interval2scm (unsmob_molecule (m)->extent (a));
}



ADD_INTERFACE (Rest,"rest-interface",
  "a rest",
  "style minimum-beam-collision-distance");

