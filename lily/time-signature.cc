/*   
  time-signature.cc --  implement Time_signature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "scalar.hh"
#include "molecule.hh"
#include "time-signature.hh"
#include "paper-def.hh"
#include "lookup.hh"

Time_signature::Time_signature ()
{
  breakable_b_ = true;
}

Molecule*
Time_signature::brew_molecule_p () const
{
  Atom s;
  if (time_sig_type_str_.length_i ())
    {
      if (time_sig_type_str_[0]=='1')
	{
	  Array<Scalar> tmparr=args_;
	  tmparr[1]=Scalar(0);
	  s = lookup_l ()->time_signature (tmparr);
	}
      else
	{
	  s = lookup_l ()-> special_time_signature (time_sig_type_str_,args_);
	}
    }
  else
    s = lookup_l ()->time_signature (args_);
  s.translate_axis (-s.extent ()[Y_AXIS].center (), Y_AXIS);
  return new Molecule (Atom (s));
}



IMPLEMENT_IS_TYPE_B1(Time_signature,Item);
