#include "molecule.hh"
#include "boxes.hh"
#include "textspanner.hh"
#include "textdef.hh"
#include "debug.hh"

Text_spanner::Text_spanner(Directional_spanner*d)
{
    support = d;
    dependencies.add(d);
}

void
Text_spanner::do_post_processing()
{
    switch(spec.align) {
    case 0:
	tpos = support->center();
	break;
    default:
	assert(false);
	break;
    }
    
    
}
Molecule*
Text_spanner::brew_molecule() const
{
    Atom tsym (spec.create(paper()));
    tsym.translate(tpos);

    Molecule*output = new Molecule;
    output->add( tsym );
    return output;
}

void
Text_spanner::print() const	// todo
{
#ifndef NDEBUG
    mtor << "Text_spanner\n";
#endif
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
    return brew_molecule()->extent().y;
}

Spanner*
Text_spanner::do_break_at(PCol*c1, PCol*c2)const
{
    return new Text_spanner(*this);    
}
