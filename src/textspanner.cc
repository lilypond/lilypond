#include "molecule.hh"
#include "boxes.hh"
#include "textspanner.hh"
#include "textdef.hh"
#include "debug.hh"

NAME_METHOD(Text_spanner);

void
Text_spanner::set_support(Directional_spanner*d)
{
    support = d;
    add_depedency(d);
}

Text_spanner::Text_spanner()
{
    support = 0;
}

void
Text_spanner::do_print() const
{
    spec.print();
}

void
Text_spanner::do_post_processing()
{
    switch(spec.align_i_) {
    case 0:
	tpos = support->center();
	break;
    default:
	assert(false);
	break;
    }
    
    
}
Molecule*
Text_spanner::brew_molecule_p() const
{
    Atom tsym (spec.create_atom(paper()));
    tsym.translate(tpos);

    Molecule*output = new Molecule;
    output->add( tsym );
    return output;
}

void
Text_spanner::do_pre_processing()
{
    right = support->right;
    left = support->left;
    assert(left && right);
}

Interval
Text_spanner::height()const
{
    return brew_molecule_p()->extent().y;
}

Spanner*
Text_spanner::do_break_at(PCol*c1, PCol*c2)const
{
    return new Text_spanner(*this); // todo
}
