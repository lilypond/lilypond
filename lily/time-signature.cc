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

Molecule*
Time_signature::do_brew_molecule_p () const
{
  SCM st = get_elt_property ("style");
  
  if (gh_string_p (st))
    {
      String style (ly_scm2string (st));
      if (style[0]=='1')
	{
	  Array<int> tmparr = args_;
	  return new Molecule( lookup_l ()->time_signature (args_[0], 0, paper_l ()));
	}
      else
	{
	  return new Molecule( lookup_l ()-> special_time_signature (style, args_[0], args_[1], paper_l ()));
	}
    }
  else
    return new Molecule(lookup_l ()->time_signature (args_[0], args_[1],paper_l ()));
}





