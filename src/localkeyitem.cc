#include "localkeyitem.hh"
#include "molecule.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paperdef.hh"
#include "request.hh"
#include "notehead.hh"

NAME_METHOD(Local_key_item);
Local_key_item::Local_key_item(int i)
{
    c0_position  = i;
}
void
Local_key_item::add(Item*head_l)
{
    group.push(head_l);
    add_depedency(head_l);
}
void
Local_key_item::add(Melodic_req*m_l)
{
    add(m_l->octave, m_l->notename, m_l->accidental);
}
void
Local_key_item::add (int o, int p , int a)
{
    Local_acc l;
    l.octave = o;
    l.name = p;
    l.acc = a;
    accs.push(l);
}

void
Local_key_item::do_pre_processing()
{
    accs.sort(Local_acc::compare);
}

Molecule*
Local_key_item::brew_molecule_p()const
{
    Molecule* output = new Molecule;
    Molecule*octmol = 0;
    int lastoct = -100;
    for  (int i = 0; i <  accs.size(); i++) {
	// do one octave
	if (accs[i].octave != lastoct) {
	    if (octmol){
		Real dy =lastoct*7*paper()->internote();
		octmol->translate(Offset(0, dy));
		output->add(*octmol);
		delete octmol;
	    }
	    octmol= new Molecule;
	}
	lastoct = accs[i].octave;
	Symbol s =paper()->lookup_p_->accidental(accs[i].acc);   
	Atom a(s);
	Real dy = (accs[i].name + c0_position) * paper()->internote();
	a.translate(Offset(0,dy));

	octmol->add_right(a);
    }

    if (octmol){
	Real dy =lastoct*7*paper()->internote();
	octmol->translate(Offset(0, dy));
	output->add(*octmol);
	delete octmol;
    }

    Interval head_width;
    for (int i = 0; i  < group.size(); i++) {
	head_width.unite(group[i]->width());
    }
    output->translate(Offset(-output->extent().x.right + head_width.left ,0));
    
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
