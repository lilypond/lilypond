/*   
  time-signature.cc --  implement Time_signature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "scalar.hh"
#include "molecule.hh"
#include "time-signature.hh"
#include "paper-def.hh"
#include "lookup.hh"

Time_signature::Time_signature ()
{
  set_elt_property (breakable_scm_sym, SCM_BOOL_T);
}

Molecule*
Time_signature::do_brew_molecule_p () const
{
  if (time_sig_type_str_.length_i ())
    {
      if (time_sig_type_str_[0]=='1')
	{
	  Array<int> tmparr = args_;
	  return new Molecule( lookup_l ()->time_signature (args_[0], 0));
	}
      else
	{
	  return new Molecule( lookup_l ()-> special_time_signature (time_sig_type_str_ ,args_[0], args_[1]));
	}
    }
  else
    return new Molecule(lookup_l ()->time_signature (args_[0], args_[1]));
}




