#include "paper.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "boxes.hh"
#include "textspanner.hh"

Text_spanner::Text_spanner(Directional_spanner*d)
{
    support = d;
    align = 0;
    style = "roman";
}

void
Text_spanner::process()
{
    Offset tpos;

    switch(align) {
    case 0:
	tpos = support->center();
	break;
    default:
	assert(false);
	break;
    }
    
    Paperdef *pap_p = paper();
    
    Atom tsym (pap_p->lookup_->text(style, text, -align));
    tsym.translate(tpos);
    output = new Molecule;
    output->add( tsym );    
}

void
Text_spanner::preprocess()
{
    right = support->right;
    left = support->left;
    assert(left && right);
}

Interval
Text_spanner::height()const
{
    return output->extent().y;
}

Spanner*
Text_spanner::broken_at(PCol*c1, PCol*c2)const
{
    Text_spanner *n=new Text_spanner(*this);
    n->left = c1;
    n->right = c2;
    return n;
}
