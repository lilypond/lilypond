/*   
  time-signature.cc --  implement Time_signature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "molecule.hh"
#include "time-signature.hh"
#include "paper-def.hh"
#include "lookup.hh"

Time_signature::Time_signature ()
{
  set_elt_property ("breakable", SCM_BOOL_T);
}

// ugh.!
Molecule*
Time_signature::do_brew_molecule_p () const
{
  SCM st = get_elt_property ("style");

  SCM frac = get_elt_property ("fraction");
  int n = 4;
  int d = 4;
  if (gh_pair_p (frac))
    {
      n = gh_scm2int (gh_car (frac));
      d = gh_scm2int (gh_cdr (frac));
    }

  
  if (gh_string_p (st))
    {
      String style (ly_scm2string (st));
      if (style[0]=='1')
	{
	  return new Molecule (time_signature (n, 0));
	}
      else
	{
	  return new Molecule (special_time_signature (style, n, d));
	}
    }
  else
    return new Molecule (time_signature (n,d));
}

Molecule
Time_signature::special_time_signature (String s, int n, int d) const
{
  // First guess: s contains only the signature style
  String symbolname = "timesig-" + s + to_str (n) + "/" + to_str (d);
  
  Molecule m = lookup_l ()->afm_find (symbolname, false);
  if (!m.empty_b()) 
    return m;

  // Second guess: s contains the full signature name
  m = lookup_l ()->afm_find ("timesig-"+s, false);
  if (!m.empty_b ()) 
    return m;

  // Resort to default layout with numbers
  return time_signature (n,d);
}


Molecule
Time_signature::time_signature (int num, int den) const
{
  String sty = "number";

  /*
    UGH: need to look at fontsize.
   */
  Molecule n (lookup_l ()->text (sty, to_str (num), paper_l ()));
  Molecule d (lookup_l ()->text (sty, to_str (den), paper_l ()));
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

