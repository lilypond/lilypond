#include "keyitem.hh"
#include "key.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper.hh"
#include "lookup.hh"


Keyitem::Keyitem(int c)
{
    c_position = c;
}

void
Keyitem::read(svec<int> s)
{
    for (int i = 0 ; i< s.sz(); ) {
	int note = s[i++];
	int acc = s[i++];
	    
	add(note, acc);
    }
}
void
Keyitem::add(int p, int a)
{
    pitch.add(p);
    acc.add(a);
}

void
Keyitem::preprocess()
{
    brew_molecole();
}

void
Keyitem::brew_molecole()
{
    output = new Molecule;
    Real inter = paper()->interline()/2;
    
    for (int i =0; i < pitch.sz(); i++) {
	Symbol s= paper()->lookup_->accidental(acc[i]);
	Atom a(s);
	a.translate(Offset(0,(c_position + pitch[i]) * inter));
	Molecule m(a);
	output->add_right(m);	
    }
    Molecule m(paper()->lookup_->fill(Box(
	Interval(0, paper()->note_width()),
	Interval(0,0))));
    output->add_right(m);	
}
