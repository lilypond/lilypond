#include "font-interface.hh"
#include "item.hh"
#include "molecule.hh"
#include "accidental-interface.hh"

/*
  TODO: insert support for smaller cautionaries, tie-break-reminders.
  Either here or in new-accidental-engraver.

  'accidentals should go, for a single 'accidental property -- see
  accidental-placement.cc

*/


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

  if (tie && !tie->original_)
    {
      me->suicide ();
    }
  return SCM_UNSPECIFIED;
}

Array<Box>
Accidental_interface::accurate_boxes (Grob *a,Grob**common)
{
  Box b;
  b[X_AXIS] = a->extent (a, X_AXIS);
  b[Y_AXIS] = a->extent (a, Y_AXIS);

  Array<Box> boxes;
  
  bool parens = false;
  if (to_boolean (a->get_grob_property ("cautionary")))
    {
      SCM cstyle = a->get_grob_property ("cautionary-style");
      parens = gh_equal_p (cstyle, ly_symbol2scm ("parentheses"));

    }

  SCM accs = a->get_grob_property ("accidentals");
  SCM scm_style = a->get_grob_property ("style");
  if (!gh_symbol_p (scm_style)
      && !parens
      && scm_ilength (accs) == 1)
    {
      if (gh_scm2int (gh_car (accs)) == -1)
	{
	  Box stem = b;
	  Box bulb = b;

	  /*
	    we could make the stem thinner, but that places the flats
	    really close.
	  */
	  stem[X_AXIS][RIGHT] *= .5;
	  bulb[Y_AXIS][UP] *= .35;

	  boxes.push (bulb);
	  boxes.push (stem);
	}
      /*
	TODO: add support for natural, double flat.
       */
    }

  if (!boxes.size())
    boxes.push (b);

  Offset o (a->relative_coordinate (common[X_AXIS],  X_AXIS),
	    a->relative_coordinate (common[Y_AXIS],  Y_AXIS));
  for(int i = boxes.size(); i--;)
    {
      boxes[i].translate(o);
    }
  
  return boxes;
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
			      scm_int2num (-1)), SCM_EOL),
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
				      to_string (gh_scm2int(entry))));
      
      mol.add_at_edge (X_AXIS,  RIGHT, acc, 0.1);
    }

  if (parens)
    mol = parenthesize (me, mol); 

  return mol.smobbed_copy();
}



ADD_INTERFACE (Accidental_interface, "accidental-interface",
	      "a single accidental",
	       "cautionary-style style tie accidentals");
