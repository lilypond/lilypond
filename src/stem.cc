#include "stem.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper.hh"
#include "notehead.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "pcol.hh"

const int STEMLEN=7;
static int
ABS(int i) {
    return (i < 0)?-i:i;
}


Stem::Stem(int c)
{
    minnote = 1000;		// invalid values
    maxnote = -1000;
    bot = top = 0;
    flag = 4;
    dir =0;
    staff_center=c;
    stemlen=0;
    print_flag=true;
    stem_xoffset=0;
}


void
Stem::set_stemend(Real se)
{
    assert(!output);
    // todo: margins
    assert((dir > 0 && se >= maxnote) || (se <= minnote && dir <0));
    
    top = (dir < 0) ? maxnote           : se;
    bot = (dir < 0) ? se  : minnote;
    flag = dir*ABS(flag);
}

void
Stem::add(Notehead *n)
{
    if (n->balltype == 1)
	return;
    int p = n->position;
    if ( p < minnote)
	minnote = p;
    if ( p> maxnote)
	maxnote = p;
}
void
Stem::print()const
{
    mtor << "Stem minmax=["<< minnote<<","<<maxnote<<"], flag: "<<flag;
    Item::print();
}

void
Stem::set_default_dir()
{
    Real mean = (minnote+maxnote)/2;
    dir = (mean > staff_center) ? -1: 1;
}

void
Stem::set_default_stemlen()
{
    if (!dir)
	set_default_dir();

    int stafftop = 2*staff_center;
    stemlen = STEMLEN  + (maxnote - minnote);
    
    // uhh... how about non 5-line staffs?	
    if (maxnote < -2 && dir == 1){
	int t = staff_center - staff_center/2; 
	stemlen = t - minnote +2;
    } else if (minnote > stafftop + 2 && dir == -1) {
	int t = staff_center + staff_center/2;
	stemlen = maxnote -t +2;
    }

    assert(stemlen);
}


void
Stem::set_default_extents()
{
    assert(minnote<=maxnote);

    if (!stemlen)
	set_default_stemlen();

    set_stemend((dir< 0) ? maxnote-stemlen: minnote +stemlen);
    if (dir > 0){	
	stem_xoffset = paper()->note_width();
    }
}

void
Stem::postprocess()
{
    set_default_extents();
    brew_molecole();
}

Interval
Stem::width()const
{
    if (!print_flag || ABS(flag) <= 4)
	return Interval(0,0);	// TODO!
    Paperdef*p= paper();
    Interval r(p->lookup_->flag(flag).dim.x);
    r+= stem_xoffset;
    return r;
}

void
Stem::brew_molecole()
{
    assert(pstaff_);
    assert(bot!=top);
    assert(!output);
    
    Paperdef *p =paper();

    Real dy = p->internote();
    Symbol ss =p->lookup_->stem(bot*dy,top*dy);

    
    output = new Molecule(Atom(ss));

    if (print_flag&&ABS(flag) > 4){
	Symbol fl = p->lookup_->flag(flag);
	Molecule m(fl);
	if (flag < -4){		
	    output->add_bot(m);
	} else if (flag > 4) {
	    output->add_top(m);
	} else
	    assert(false); 
    }

    output->translate(Offset(stem_xoffset,0));

}

Real
Stem::hpos()const
{
    return pcol_->hpos + stem_xoffset;
}


void
Stem::preprocess()
{
 
}
