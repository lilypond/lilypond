#include "slur.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper.hh"
#include "notehead.hh"
#include "pcol.hh"
#include "molecule.hh"
#include "debug.hh"
#include "boxes.hh"

void
Slur::add(Notehead*n)
{
    encompass.add(n);
    dir = 0;
    open_right=open_left=false;
}

Interval
Slur::height() const
{
    return Interval(0,0);	// todo
}

void
Slur::set_default_dir()
{
    int sumpos=0;
    for (int i=0; i < encompass.sz(); i ++) {
	sumpos += encompass[i]->position;
    }

    /* should consult stems */
    Real meanpos = sumpos/Real(encompass.sz());
    if (meanpos < 5)		// todo
	dir = -1;
    else
	dir = 1;    
}

void
Slur::print()const
{
    mtor << "Slur.\n";   
}

void
Slur::preprocess()
{
    right  = encompass.last()->pcol_;
    left = encompass[0]->pcol_;    
}

Spanner*
Slur::broken_at(const PCol*l, const PCol*r) const
{
    assert(l->line == r->line);
    Slur*ret = new Slur(*this);
    ret->encompass.set_size(0);
    for (int i =0; i < encompass.sz(); i++) {
	if (encompass[i]->pcol_->line==l->line)
	    ret->encompass.add(encompass[i]);
    }
    if (right != r)
	ret->open_right = true;
    if (left != l)
	ret->open_left = true;

    ret->left = l;
    ret->right = r;
    return ret;
}

void
Slur::process()
{
    set_default_dir();
    brew_molecule();
}

void
Slur::brew_molecule()
{
    output = new Molecule;
    assert(left->line == right->line);
    int minp=1000, maxp=-1000;	// todo
    for (int i=0; i<encompass.sz(); i++) {
	minp = encompass[i]->position <? minp;
	maxp = encompass[i]->position <? maxp;
    }    
    assert(encompass.sz()>0);	// todo
    int pos1 = encompass.last()->position;
    int pos2 = encompass[0]->position;

    int dy =  pos1-pos2;

    Real w = width().length();
    Real nw = paper()->note_width();
    w -= nw;
    Symbol sl = paper()->lookup_->slur(dy , w, dir);
    Atom a(sl);
    a.translate(Offset(nw,pos2*paper()->internote()));
    output->add(a);
}

