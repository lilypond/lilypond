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
  if (bt == 0)
    {
      me->translate_axis (Staff_symbol_referencer::staff_space (me) , Y_AXIS);
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
Rest::glyph_name (Grob * me, int balltype, String style)
{
  bool ledger_b =false;

  if (balltype == 0 || balltype == 1)
    {
      Real rad = Staff_symbol_referencer::staff_radius (me) * 2.0;
      Real pos = Staff_symbol_referencer::position_f (me);

      /*
	Figure out when the rest is far enough outside the staff. This
	could bemore generic, but hey, we understand this even after
	dinner.
	
       */
      ledger_b = ledger_b || (balltype == 0 && (pos >= rad +2   || pos < -rad ));
      ledger_b = ledger_b || (balltype == 1 &&
			      (pos  <= -rad -2 || pos > rad));
    }

  return ("rests-") + to_str (balltype)
    + (ledger_b ? "o" : "") + style;
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
  SCM style_sym =me->get_grob_property ("style");
  if (gh_symbol_p (style_sym))
    {
      style = ly_scm2string (scm_symbol_to_string (style_sym));
    }

  for(;;) {
    String idx = glyph_name (me, balltype, style);
    Molecule res = Font_interface::get_default_font (me)->find_by_name (idx);
    if(res.empty_b() && style!="")
      style="";
    else	      
      return res.smobbed_copy();
  }
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

bool
Rest::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("rest-interface"));
}
