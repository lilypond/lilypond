/*
  molecule.cc -- implement Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "interval.hh"
#include "string.hh"
#include "molecule.hh"
#include "atom.hh"
#include "debug.hh"

Box
Molecule::extent() const
{
  Box b;
  for (iter_top (atoms_,c); c.ok(); c++)
    b.unite (c->extent());
  return b;
}

void
Molecule::translate (Offset o)
{
  for (iter_top (atoms_,c); c.ok(); c++)
    c->translate (o);
}

void
Molecule::translate_axis (Real x,Axis a)
{
  for (iter_top (atoms_,c); c.ok(); c++)
    c->translate_axis (x,a);
}

void
Molecule::add_molecule (Molecule const &m)
{
  for (iter_top (m.atoms_,c); c.ok(); c++) 
    {
      add_atom (**c);
    }
}


void
Molecule::add_at_edge (Axis a, Direction d, Molecule const &m)
{
  if (!atoms_.size()) 
    {
      add_molecule (m);
      return;
    }
  Real offset = extent ()[a][d] - m.extent ()[a][-d];
  Molecule toadd (m);
  toadd.translate_axis (offset, a);
  add_molecule (toadd);
}

  
  
void
Molecule::operator = (Molecule const &)
{
  assert (false);
}

Molecule::Molecule (Molecule const &s)
{
  add_molecule (s);
}

void
Molecule::print() const
{
#ifndef NPRINT
  if (! check_debug)
    return;
  for (iter_top (atoms_,c); c.ok(); c++)
    c->print();
#endif
}

void
Molecule::add_atom (Atom const &a)
{
  atoms_.bottom().add (new Atom (a)); 
}

Molecule::Molecule (Atom const &a)
{
  add_atom (a) ;
}
