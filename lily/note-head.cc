/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>

#include "misc.hh"
#include "dots.hh"
#include "note-head.hh"
#include "debug.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "musical-request.hh"

#include "staff-symbol-referencer.hh"

/*
  build a ledger line for small pieces.
 */
Molecule
Note_head::ledger_line (Interval xwid, Grob *me) 
{
  Drul_array<Molecule> endings;
  endings[LEFT] = Font_interface::get_default_font (me)->find_by_name ("noteheads-ledgerending");
  Molecule *e = &endings[LEFT];
  endings[RIGHT] = *e;
  
  Real thick = e->extent (Y_AXIS).length ();
  Real len = e->extent (X_AXIS).length () - thick;

  Molecule total;
  Direction d = LEFT;
  do {
    endings[d].translate_axis (xwid[d] - endings[d].extent (X_AXIS)[d], X_AXIS);
    total.add_molecule (endings[d]);    
  } while ((flip (&d)) != LEFT);

  Real xpos = xwid [LEFT] + len;

  while (xpos + len + thick /2 <= xwid[RIGHT])
    {
      e->translate_axis (len, X_AXIS);
      total.add_molecule (*e);
      xpos += len;
    }

  return total;
}


MAKE_SCHEME_CALLBACK (Note_head,brew_molecule,1);

SCM
Note_head::brew_molecule (SCM smob)  
{
  Grob *me = unsmob_grob (smob);

  
  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;
  int sz = Staff_symbol_referencer::line_count (me)-1;
  int p = (int)  rint (Staff_symbol_referencer::position_f (me));
  int streepjes_i = abs (p) < sz 
    ? 0
    : (abs (p) - sz) /2;

  SCM style  = me->get_grob_property ("style");
  if (!gh_symbol_p (style))
    {
      return SCM_EOL;
    }

  /*
    ugh: use gh_call ()

    UGH: use grob-property.
  */
  Molecule out = Font_interface::get_default_font (me)->find_by_name (String ("noteheads-") + 
		ly_scm2string (scm_eval2 (gh_list (ly_symbol2scm ("find-notehead-symbol"),
						  me->get_grob_property ("duration-log"),
						  ly_quote_scm (style),
						  SCM_UNDEFINED),
					  SCM_EOL)));

  if (streepjes_i) 
    {
      Direction dir = (Direction)sign (p);
      Interval hd = out.extent (X_AXIS);
      Real hw = hd.length ()/4;
      Molecule ledger (ledger_line (Interval (hd[LEFT] - hw,
					       hd[RIGHT] + hw), me));
      

      ledger.set_empty (true);
      Real offs = (Staff_symbol_referencer::on_staffline (me))
	? 0.0
	: -dir * inter_f;

      for (int i=0; i < streepjes_i; i++)
	{
	  Molecule s (ledger);
	  s.translate_axis (-dir * inter_f * i*2 + offs,
			    Y_AXIS);
	  out.add_molecule (s);
	}
    }
  return out.smobbed_copy ();
}

bool
Note_head::has_interface (Grob*m)
{
  return m&& m->has_interface (ly_symbol2scm ("note-head-interface"));
}


MAKE_SCHEME_CALLBACK (Note_head,brew_ez_molecule,1);

SCM
Note_head::brew_ez_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  int l = gh_scm2int (me->get_grob_property ("duration-log"));

  int b = (l >= 2);
  SCM at = gh_list (ly_symbol2scm ("ez-ball"),
		    me->get_grob_property ("note-character"),
		    gh_int2scm (b),
		    gh_int2scm (1-b),
		    SCM_UNDEFINED);
  Box bx (Interval (0, 1.0), Interval (-0.5, 0.5));
  Molecule m (bx, at);

  return m.smobbed_copy ();
}


Real
Note_head::stem_attachment_coordinate (Grob *me, Axis a)
{
  SCM v = me->get_grob_property ("stem-attachment-function");

  if (!gh_procedure_p (v))
    return 0.0;

  SCM st = me->get_grob_property ("style");
  SCM result = gh_apply (v, gh_list (st, SCM_UNDEFINED));

  if (!gh_pair_p (result))
    return 0.0;

  result = (a == X_AXIS) ? gh_car (result) : gh_cdr (result);
  
  return gh_number_p (result) ?  gh_scm2double (result) : 0.0;
}
