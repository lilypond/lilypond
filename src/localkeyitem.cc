#include "localkeyitem.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "paper.hh"

Local_key_item::Local_key_item(int i)
{
    c0_position  = i;
}

void
Local_key_item::add (int o, int p , int a)
{
    Local_acc l;
    l.octave = o;
    l.name = p;
    l.acc = a;
    accs.add(l);
}

void
Local_key_item::preprocess()
{
    brew_molecole();
}

void
Local_key_item::brew_molecole()
{
    accs.sort(Local_acc::compare);

    output = new Molecule;
    Molecule*octmol = 0;
    int lastoct = -100;
    for  (int i = 0; i <  accs.sz(); i++) {
	if (accs[i].octave != lastoct) {
	    if (octmol){
		Real dy =lastoct*7*paper()->interline()/2;
		octmol->translate(Offset(0, dy));
		output->add(*octmol);
		delete octmol;
	    }
	    octmol= new Molecule;
	}
	lastoct = accs[i].octave;
	Symbol s =paper()->lookup_->accidental(accs[i].acc);   
	Atom a(s);
	Real dy = (accs[i].name + c0_position) * paper()->interline()/2;
	a.translate(Offset(0,dy));

	octmol->add_right(a);
    }

    if (octmol){
	Real dy =lastoct*7*paper()->interline()/2;
	octmol->translate(Offset(0, dy));
	output->add(*octmol);
	delete octmol;
    }
}

int
Local_acc::compare(Local_acc&a, Local_acc&b)
{
    if (a.octave - b.octave)
	return a.octave - b.octave;
    if (a.name - b.name)
	return a.name - b.name;
    
    assert(false);
};
