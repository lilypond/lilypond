#include "glob.hh"
#include "string.hh"
#include "molecule.hh"
#include "symbol.hh"

Box
Atom::extent() const
{
    Box b( sym->dim);
    b.translate(off);
    return b;
}

Atom::Atom(const Symbol * s)
{
    sym=s;
}

String
Atom::TeXstring() const
{
    // whugh.. Hard coded...
    String s("\\raise");
    s+= String(off.y * VERT_TO_PT)+"pt\\hbox to 0pt{\\kern ";
    s+= String(off.x * HOR_TO_PT) + "pt";
    s+= sym->tex + "\\hss}";
    return s;
}

/****************************************************************/

String
Molecule::TeXstring() const
{
    String s;
    for(Cursor<Atom> c(ats); c.ok(); c++)
	s+=(*c).TeXstring();
    return s;
}

Box
Molecule::extent() const
{
    Box b;
    for(Cursor<Atom> c(ats); c.ok(); c++)
	b.unite((*c).extent());
    return b;
}

void
Molecule::translate(Offset o)
{
    for(Cursor<Atom> c(ats); c.ok(); c++)
	(*c).translate(o);
}

void
Molecule::add(const Molecule &m)
{
    for (Cursor<Atom> c(m.ats); c.ok(); c++) {
	Atom a(c);
	ats.bottom().add(a);    
    }
}

void
Molecule::add_right(const Molecule &m)
{
    Real xof=extent().x.max - m.extent().x.min;
    Molecule toadd(m);
    toadd.translate(Offset(xof, 0.0));
    add(toadd);
}

void
Molecule::add_left(const Molecule &m)
{
    Real xof=extent().x.min - m.extent().x.max;
    Molecule toadd(m);
    toadd.translate(Offset(xof, 0.0));
        add(toadd);
}


void
Molecule::add_top(const Molecule &m)
{
    Real yof=extent().y.max - m.extent().y.min;
    Molecule toadd(m);
    toadd.translate(Offset(0,yof));
        add(toadd);
}

void
Molecule::add_bot(const Molecule &m)
{
    Real yof=extent().y.min- m.extent().y.max;
    Molecule toadd(m);
    toadd.translate(Offset(0,yof));
    add(toadd);
}

