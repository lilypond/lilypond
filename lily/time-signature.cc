/*   
  time-signature.cc --  implement Time_signature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "molecule.hh"
#include "text-item.hh"
#include "time-signature.hh"
#include "paper-def.hh"
#include "font-interface.hh"

MAKE_SCHEME_CALLBACK(Time_signature,brew_molecule,1);
/*
  TODO: make different functions for special and normal timesigs.
 */
SCM
Time_signature::brew_molecule (SCM smob) 
{
  Grob * me = unsmob_element (smob);
  SCM st = me->get_grob_property ("style");
  SCM frac = me->get_grob_property ("fraction");
  int n = 4;
  int d = 4;
  if (gh_pair_p (frac))
    {
      n = gh_scm2int (gh_car (frac));
      d = gh_scm2int (gh_cdr (frac));
    }

  
  if (gh_symbol_p (st))
    {
      String style (ly_scm2string (scm_symbol_to_string (st)));
      if (style[0]=='1')
	{
	  return time_signature (me, n, 0).smobbed_copy ();
	}
      else
	{
	  return special_time_signature (me, style, n, d).smobbed_copy ();
	}
    }
  else
    return time_signature (me, n,d).smobbed_copy ();
}

Molecule
Time_signature::special_time_signature (Grob*me, String s, int n, int d)
{
  /*
    Randomly probing the font sucks?
  */
  
  SCM alist_chain = Font_interface::font_alist_chain (me);
  
  SCM style_chain =
    Font_interface::add_style (me, ly_symbol2scm ("timesig-symbol"),
			       alist_chain);

  Font_metric *feta = Font_interface::get_font (me, style_chain);

  /*
    First guess: s contains only the signature style, append fraction.
  */
  String symbolname = "timesig-" + s + to_str (n) + "/" + to_str (d);
  
  Molecule m = feta->find_by_name (symbolname);
  if (!m.empty_b()) 
    return m;

  /*
    Second guess: s contains the full signature name
  */
  m = feta->find_by_name ("timesig-" + s);
  if (!m.empty_b ()) 
    return m;

  // Resort to default layout with numbers
  return time_signature (me, n, d);
}


Molecule
Time_signature::time_signature (Grob*me,int num, int den)
{
  SCM chain = Font_interface::font_alist_chain (me);

  Molecule n = Text_item::text2molecule (me,
					 ly_str02scm (to_str (num).ch_C ()),
					 chain);
  Molecule d = Text_item::text2molecule (me,
					 ly_str02scm (to_str (den).ch_C ()),
					 chain);
  n.align_to (X_AXIS, CENTER);
  d.align_to (X_AXIS, CENTER);
  Molecule m;
  if (den)
    {
      m.add_at_edge (Y_AXIS, UP, n, 0.0);
      m.add_at_edge (Y_AXIS, DOWN, d, 0.0);
    }
  else
    {
      m = n;
      m.align_to (Y_AXIS, CENTER);
    }
  return m;
}

