#include "slur.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper.hh"
#include "notehead.hh"
#include "pcol.hh"
#include "molecule.hh"
#include "debug.hh"
#include "boxes.hh"

Slur::Slur()
{
    dir = 0;
    open_right=open_left=false;
}

Offset
Slur::center() const
{
    int pos1 = encompass.last()->position;
    int pos2 = encompass[0]->position;

    int dy =  pos1-pos2;

    Real w = width().length();

    return Offset(w/2,dy * paper()->internote());
}

void
Slur::add(Notehead*n)
{
    encompass.add(n);
    dependencies.add(n);
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
Slur::do_pre_processing()
{
    right  = encompass.last()->pcol_;
    left = encompass[0]->pcol_;    
}

Spanner*
Slur::do_break_at(PCol*l, PCol*r) const
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


    return ret;
}

void
Slur::do_post_processing()
{
    set_default_dir();
}

Molecule*
Slur::brew_molecule() const
{
    Molecule*output = new Molecule;
    assert(left->line == right->line);
    int minp=1000, maxp=-1000;	// todo    
    for (int i=0; i<encompass.sz(); i++) {
	minp = encompass[i]->position <? minp;
	maxp = encompass[i]->position >? maxp;
    }
    assert(encompass.sz()>0);	// todo
    
    Notehead *lnote_p =encompass[0];
    Notehead *rnote_p =encompass.last();
    int pos1 = lnote_p->position;
    int pos2 = rnote_p->position;

    int dy =  pos2-pos1;
    Real nw_f = paper()->note_width();
    Real w = width().length();
    w+= (-lnote_p->x_dir + rnote_p->x_dir)* nw_f ;
    Real round_w = w;		// slur lookup rounds the slurwidth .
    
    Symbol sl = paper()->lookup_->slur(dy , round_w, dir);

    Real error = w-round_w;
    
    Atom a(sl);
    a.translate(Offset((lnote_p->x_dir + 0.5 )*nw_f + error/2,
		       (pos2+2*dir) * paper()->internote()));
    output->add(a);
    return output;
}

