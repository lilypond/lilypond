/*   
  multi-measure-rest.cc --  implement Multi_measure_rest
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Jan Nieuwenhuizen <jan@digicash.com>
  
 */

#include "multi-measure-rest.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "dimen.hh"
#include "rest.hh"
#include "script.hh"
#include "text-def.hh"
#include "molecule.hh"

IMPLEMENT_IS_TYPE_B1 (Multi_measure_rest, Item);

Multi_measure_rest::Multi_measure_rest ()
{
  measures_i_ = 0;
}

void
Multi_measure_rest::do_print () const
{
  DOUT << "measures_i_ " << measures_i_;
}

Molecule*
Multi_measure_rest::brew_molecule_p () const
{
  /*
   [TODO]                                     3
     * make real multi-measure rest symbol: |---|
     * make two,four,eight-measure-rest symbols
   */

  Atom s (paper ()->lookup_l ()->rest (0, 0));
  Molecule* mol_p = new Molecule ( Atom (s));
  Real interline_f = paper ()->interline_f ();
  mol_p->translate_axis (interline_f, Y_AXIS);

  if (measures_i_)
    {
      Text_def text;
      text.text_str_ = measures_i_;
      text.style_str_ = "number";
      Atom s = text.get_atom (paper (), UP);
      s.translate_axis (3.0 * interline_f, Y_AXIS);
      mol_p->add (s);
    }

  return mol_p;
}

