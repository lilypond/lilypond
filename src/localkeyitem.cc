#include "localkeyitem.hh"
#include "molecule.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper.hh"
#include "notehead.hh"
Local_key_item::Local_key_item(int i)
{
    c0_position  = i;
}

void
Local_key_item::add (int o, int p , int a,Notehead*head_p)
{
    Local_acc l;
    l.octave = o;
    l.name = p;
    l.acc = a;
    accs.add(l);
    group.add(head_p);
    dependencies.add(head_p);
}

void
Local_key_item::do_pre_processing()
{
    accs.sort(Local_acc::compare);
}
Molecule*
Local_key_item::brew_molecule_p()const
{

    Molecule*    output = new Molecule;
    Molecule*octmol = 0;
    int lastoct = -100;
    for  (int i = 0; i <  accs.size(); i++) {
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
	Symbol s =paper()->lookup_p_->accidental(accs[i].acc);   
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
    return output;
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
