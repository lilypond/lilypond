#include "scalar.hh"
#include "molecule.hh"
#include "time-signature.hh"
#include "paper-def.hh"
#include "lookup.hh"

Time_signature::Time_signature (Array<Scalar>a)
  :args (a)
{
  breakable_b_ = true;
}

Molecule*
Time_signature::brew_molecule_p() const
{
  Atom s = lookup_l ()->time_signature (args);
  s.translate_axis (-s.extent()[Y_AXIS].center (), Y_AXIS);
  return new Molecule (Atom (s));
}



IMPLEMENT_IS_TYPE_B1(Time_signature,Item);
