/*
  molecule.cc -- implement Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "interval.hh"
#include "dimen.hh"
#include "string.hh"
#include "molecule.hh"
#include "atom.hh"
#include "debug.hh"
#include "tex.hh"

String
Molecule::TeX_string() const
{
  String s;
  for (iter_top (ats,c); c.ok(); c++)
    s+=c->TeX_string();
  return s;
}

Box
Molecule::extent() const
{
  Box b;
  for (iter_top (ats,c); c.ok(); c++)
    b.unite (c->extent());
  return b;
}

void
Molecule::translate (Offset o)
{
  for (iter_top (ats,c); c.ok(); c++)
    c->translate (o);
}

void
Molecule::translate_axis (Real x,Axis a)
{
  for (iter_top (ats,c); c.ok(); c++)
    c->translate_axis (x,a);
}

void
Molecule::add (Molecule const &m)
{
  for (iter_top (m.ats,c); c.ok(); c++) 
    {
      add (**c);
    }
}


void
Molecule::add_at_edge (Axis a, Direction d, Molecule const &m)
{
  if (!ats.size()) 
    {
      add (m);
      return;
    }
  Real offset = extent ()[a][d] - m.extent ()[a][-d];
  Molecule toadd (m);
  toadd.translate_axis (offset, a);
  add (toadd);
}

  
  
void
Molecule::operator = (Molecule const &)
{
  assert (false);
}

Molecule::Molecule (Molecule const &s)
{
  add (s);
}

void
Molecule::print() const
{
#ifndef NPRINT
  if (! check_debug)
    return;
  for (iter_top (ats,c); c.ok(); c++)
    c->print();
#endif
}

void
Molecule::add (Atom const &a)
{
  ats.bottom().add (new Atom (a)); 
}
