/*   
  time-signature.cc --  implement Time_signature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "molecule.hh"
#include "text-item.hh"
#include "time-signature.hh"
#include "paper-def.hh"
#include "lookup.hh"

MAKE_SCHEME_CALLBACK(Time_signature,brew_molecule,1);

SCM
Time_signature::brew_molecule (SCM smob) 
{
  Score_element * me = unsmob_element (smob);
  SCM st = me->get_elt_property ("style");
  SCM frac = me->get_elt_property ("fraction");
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
	  return time_signature (me, n, 0).create_scheme();
	}
      else
	{
	  return special_time_signature (me, style, n, d).create_scheme();
	}
    }
  else
    return time_signature (me, n,d).create_scheme();
}

Molecule
Time_signature::special_time_signature (Score_element*me, String s, int n, int d)
{
  // First guess: s contains only the signature style
  String symbolname = "timesig-" + s + to_str (n) + "/" + to_str (d);
  
  Molecule m = me->lookup_l ()->afm_find (symbolname, false);
  if (!m.empty_b()) 
    return m;

  // Second guess: s contains the full signature name
  m = me->lookup_l ()->afm_find ("timesig-"+s, false);
  if (!m.empty_b ()) 
    return m;

  // Resort to default layout with numbers
  return time_signature (me, n,d);
}


Molecule
Time_signature::time_signature (Score_element*me,int num, int den)
{
  /*
    UGH: need to look at fontsize.
    TODO: specify using scm markup.
   */
  SCM properties = gh_append2 (me->immutable_property_alist_,
			       me->mutable_property_alist_);
  Molecule n = Text_item::text2molecule (me,
					 ly_str02scm (to_str (num).ch_C ()),
					 properties);
  Molecule d = Text_item::text2molecule (me,
					 ly_str02scm (to_str (den).ch_C ()),
					 properties);
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

