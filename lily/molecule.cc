/*
  molecule.cc -- implement Molecule

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "interval.hh"
#include "dimen.hh"
#include "string.hh"
#include "molecule.hh"
#include "symbol.hh"
#include "debug.hh"
#include "tex.hh"


/* *************** */

String
Molecule::TeX_string() const
{
    String s;
    for(iter_top(ats,c); c.ok(); c++)
	s+=c->TeX_string();
    return s;
}

Box
Molecule::extent() const
{
    Box b;
    for(iter_top(ats,c); c.ok(); c++)
	b.unite(c->extent());
    return b;
}

void
Molecule::translate(Offset o)
{
    for (iter_top(ats,c); c.ok(); c++)
	c->translate(o);
}

void
Molecule::translate_x(Real x)
{
    translate(Offset(x,0));
}

void
Molecule::translate_y(Real y)
{
    translate(Offset(0,y));
}

void
Molecule::add(Molecule const &m)
{
    for (iter_top(m.ats,c); c.ok(); c++) {
	add(**c);
    }
}

void
Molecule::add_right(Molecule const &m)
{
     if (!ats.size()) {
	add(m);
	return;
    }
   Real xof=extent().x.right - m.extent().x.left;
    Molecule toadd(m);
    toadd.translate(Offset(xof, 0.0));
    add(toadd);
}

void
Molecule::add_left(Molecule const &m)
{
    if (!ats.size()) {
	add(m);
	return;
    }
    Real xof=extent().x.left - m.extent().x.right;
    Molecule toadd(m);
    toadd.translate(Offset(xof, 0.0));
    add(toadd);
}


void
Molecule::add_top(Molecule const &m)
{
      if (!ats.size()) {
	add(m);
	return;
    }
  Real yof=extent().y.right - m.extent().y.left;
    Molecule toadd(m);
    toadd.translate_y(yof);
    add(toadd);
}

void
Molecule::add_bottom(Molecule const &m)
{
    if (!ats.size()) {
	add(m);
	return;
    }
    Real yof=extent().y.left- m.extent().y.right;
    Molecule toadd(m);
    toadd.translate_y(yof);
    add(toadd);
}

void
Molecule::operator = (Molecule const &)
{
    assert(false);
}

Molecule::Molecule(Molecule const &s)
{
    add(s);
}

void
Molecule::print() const
{
    for (iter_top(ats,c); c.ok(); c++)
	c->print();
}

void
Molecule::add(Atom const &a)
{
    ats.bottom().add(new Atom(a)); 
}
