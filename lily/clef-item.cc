/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "clef-item.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "clef-reg.hh"



Clef_item::Clef_item()
{
    change = true;
    read("violin");
}

void
Clef_item::read(String t)
{
    type_= t;
    if (type_ == "violin")
	y_off = 2;
    if (type_ == "alto")
	y_off = 4;
    if (type_ == "tenor")
	y_off = 6;
    if (type_ == "bass")
	y_off = 6;
}
void
Clef_item::read(Clef_register const &k)
{
    read(k.clef_type_str_);
}

Molecule*
Clef_item::brew_molecule_p()const
{
    String t = type_;
    if  (change)
	t += "_change";
    Symbol s = paper()->lookup_l()->clef(t);
    Molecule*output = new Molecule(Atom(s));
    output->translate(Offset(0, paper()->internote_f() * y_off));
    return output;
}

IMPLEMENT_STATIC_NAME(Clef_item);
