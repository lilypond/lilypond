#include "font-interface.hh"
#include "item.hh"
#include "molecule.hh"

/*
  TODO: insert support for smaller cautionaries, tie-break-reminders.
  Either here or in new-accidental-engraver.

  'accidentals should go, for a single 'accidental property -- see
  accidental-placement.cc

*/
class Accidental_interface
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));  
  static bool has_interface (Grob*);
};

Molecule
parenthesize (Grob*me, Molecule m)
{
  Molecule open = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-leftparen"));
  Molecule close = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-rightparen"));
  m.add_at_edge (X_AXIS, LEFT, Molecule (open), 0);
  m.add_at_edge (X_AXIS, RIGHT, Molecule (close), 0);

  return m;
}


MAKE_SCHEME_CALLBACK (Accidental_interface,after_line_breaking,1);
SCM
Accidental_interface::after_line_breaking (SCM smob)
{
  Grob *me  = unsmob_grob (smob);
  Grob *tie = unsmob_grob (me->get_grob_property ("tie"));

  if (tie && !tie->original_l_)
    {
      me->suicide ();
    }
  return SCM_UNSPECIFIED;
}
  
MAKE_SCHEME_CALLBACK (Accidental_interface,brew_molecule,1);
SCM
Accidental_interface::brew_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  bool smaller = false;
  bool parens = false;
  
  bool caut  = to_boolean (me->get_grob_property ("cautionary"));
  if (caut)
    {
      SCM cstyle = me->get_grob_property ("cautionary-style");
      parens = gh_equal_p (cstyle, ly_symbol2scm ("parentheses"));
      smaller = gh_equal_p (cstyle, ly_symbol2scm ("smaller"));
    }
  
  SCM scm_style = me->get_grob_property ("style");
  String style;
  if (gh_symbol_p (scm_style))
    {
      style = ly_scm2string (scm_symbol_to_string (scm_style));
    }
  else
    {
      /*
	preferably no name for the default style.
       */
      style = "";
    }


  Font_metric *fm = 0;
  if (smaller)
    {
      SCM ac = Font_interface::font_alist_chain (me);
      ac = gh_cons (gh_cons (gh_cons
			     (ly_symbol2scm ("font-relative-size"),
			      gh_int2scm (-1)), SCM_EOL),
		    ac);
      fm = Font_interface::get_font (me, ac);
    }
  else
    fm = Font_interface::get_default_font (me);

  Molecule mol;
  for (SCM s = me->get_grob_property ("accidentals");
       gh_pair_p (s);  s= gh_cdr (s))
    {
      SCM entry  = gh_car (s);
      
      
      Molecule acc (fm->find_by_name (String ("accidentals-") +
				      style +
				      to_str (gh_scm2int(entry))));
      
      mol.add_at_edge (X_AXIS,  RIGHT, acc, 0.1);
    }

  if (parens)
    mol = parenthesize (me, mol); 

  return mol.smobbed_copy();
}



ADD_INTERFACE(Accidental_interface, "accidental-interface",
	      "a single accidental",
	      "style tie accidentals");
