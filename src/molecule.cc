#include "varray.hh"
#include "interval.hh"
#include "dimen.hh"
#include "string.hh"
#include "molecule.hh"
#include "symbol.hh"
#include "debug.hh"
#include "tex.hh"

void
Atom::print() const
{
    mtor << "texstring: " <<sym.tex<<"\n";    
}

Box
Atom::extent() const
{
    Box b( sym.dim);
    b.translate(off);
    return b;
}

Atom::Atom(Symbol s)
{
    sym=s;
}


String
Atom::TeXstring() const
{
    // whugh.. Hard coded...
    String s("\\placebox{%}{%}{%}");
    Array<String> a;
    a.push(print_dimen(off.y));
    a.push(print_dimen(off.x));
    a.push(sym.tex);
    return substitute_args(s, a);
}

/****************/

String
Molecule::TeXstring() const
{
    String s;
    for(iter_top(ats,c); c.ok(); c++)
	s+=c->TeXstring();
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
Molecule::add(const Molecule &m)
{
    for (iter_top(m.ats,c); c.ok(); c++) {
	add(**c);
    }
}

void
Molecule::add_right(const Molecule &m)
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
Molecule::add_left(const Molecule &m)
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
Molecule::add_top(const Molecule &m)
{
      if (!ats.size()) {
	add(m);
	return;
    }
  Real yof=extent().y.right - m.extent().y.left;
    Molecule toadd(m);
    toadd.translate(Offset(0,yof));
    add(toadd);
}

void
Molecule::add_bottom(const Molecule &m)
{
    if (!ats.size()) {
	add(m);
	return;
    }
    Real yof=extent().y.left- m.extent().y.right;
    Molecule toadd(m);
    toadd.translate(Offset(0,yof));
    add(toadd);
}

void
Molecule::operator = (const Molecule&)
{
    assert(false);
}

Molecule::Molecule(const Molecule&s)
{
    add(s);
}

void
Molecule::print() const
{
    for (iter_top(ats,c); c.ok(); c++)
	c->print();
}
