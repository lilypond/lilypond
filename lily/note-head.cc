/*
  notehead.cc -- implement Note_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "misc.hh"
#include "dots.hh"
#include "note-head.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "musical-request.hh"

#include "staff-symbol-referencer.hh"

/*
  build a ledger line for small pieces.
 */
Molecule
Note_head::ledger_line (Interval xwid, Score_element *me) 
{
  Drul_array<Molecule> endings;
  endings[LEFT] = me->lookup_l()->afm_find ("noteheads-ledgerending");
  Molecule *e = &endings[LEFT];
  endings[RIGHT] = *e;
  
  Real thick = e->extent (Y_AXIS).length();
  Real len = e->extent (X_AXIS).length () - thick;

  Molecule total;
  Direction d = LEFT;
  do {
    endings[d].translate_axis (xwid[d] - endings[d].extent (X_AXIS)[d], X_AXIS);
    total.add_molecule (endings[d]);    
  } while ((flip(&d)) != LEFT);

  Real xpos = xwid [LEFT] + len;

  while (xpos + len + thick /2 <= xwid[RIGHT])
    {
      e->translate_axis (len, X_AXIS);
      total.add_molecule (*e);
      xpos += len;
    }

  return total;
}


MAKE_SCHEME_CALLBACK(Note_head,brew_molecule);

SCM
Note_head::brew_molecule (SCM smob)  
{
  Score_element *me = unsmob_element (smob);

  
  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;
  int sz = Staff_symbol_referencer::line_count (me)-1;
  int p = (int)Staff_symbol_referencer::position_f (me);
  int streepjes_i = abs (p) < sz 
    ? 0
    : (abs(p) - sz) /2;

  SCM style  = me->get_elt_property ("style");
  if (!gh_symbol_p (style))
    {
      return SCM_EOL;
    }

  // ugh: use gh_call ()
  Molecule out = me->lookup_l()->afm_find (String ("noteheads-") + 
		ly_scm2string (scm_eval2 (gh_list (ly_symbol2scm("find-notehead-symbol"),
						  me->get_elt_property ("duration-log"),
						  ly_quote_scm(style),
						  SCM_UNDEFINED),
					  SCM_EOL)));

  if (streepjes_i) 
    {
      Direction dir = (Direction)sign (p);
      Interval hd = out.extent (X_AXIS);
      Real hw = hd.length ()/4;
      Molecule ledger (ledger_line  (Interval (hd[LEFT] - hw,
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
  return out.create_scheme();
}

bool
Note_head::has_interface (Score_element*m)
{
  return m&& m->has_interface (ly_symbol2scm ("note-head-interface"));
}
