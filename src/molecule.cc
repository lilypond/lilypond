#include "glob.hh"
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
    svec<String> a;
    a.add(print_dimen(off.y));
    a.add(print_dimen(off.x));
    a.add(sym.tex);
    return substitute_args(s, a);
}


String
Molecule::TeXstring() const
{
    String s;
    for(PCursor<Atom*> c(ats); c.ok(); c++)
	s+=c->TeXstring();
    return s;
}

Box
Molecule::extent() const
{
    Box b;
    for(PCursor<Atom*> c(ats); c.ok(); c++)
	b.unite(c->extent());
    return b;
}

void
Molecule::translate(Offset o)
{
    for (PCursor<Atom*> c(ats); c.ok(); c++)
	c->translate(o);
}

void
Molecule::add(const Molecule &m)
{
    for (PCursor<Atom*> c(m.ats); c.ok(); c++) {
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
   Real xof=extent().x.max - m.extent().x.min;
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
    Real xof=extent().x.min - m.extent().x.max;
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
  Real yof=extent().y.max - m.extent().y.min;
    Molecule toadd(m);
    toadd.translate(Offset(0,yof));
    add(toadd);
}

void
Molecule::add_bot(const Molecule &m)
{
    if (!ats.size()) {
	add(m);
	return;
    }
    Real yof=extent().y.min- m.extent().y.max;
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
    for (PCursor<Atom*> c(ats); c.ok(); c++)
	c->print();
}
