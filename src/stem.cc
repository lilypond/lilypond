#include "stem.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper.hh"
#include "notehead.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "pcol.hh"
#include "misc.hh"

const int STEMLEN=7;

Stem::Stem(int c, Moment len)
{
    note_length = len;
    beams_left = 0;
    beams_right = 0;
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
Stem::print() const
{
#ifndef NPRINT
    mtor << "{\n";
    mtor << "flag "<< flag << " print_flag " << print_flag
	 << "min,max [" << minnote << ", " << maxnote << "]";
	

    Item::print();
    mtor << "}\n";
#endif
}
void
Stem::set_stemend(Real se)
{

    // todo: margins
    assert((dir > 0 && se >= maxnote) || (se <= minnote && dir <0));
    
    top = (dir < 0) ? maxnote           : se;
    bot = (dir < 0) ? se  : minnote;
    flag = dir*abs(flag);
}

void
Stem::add(Notehead *n)
{
    assert(status < PRECALCED);
    
    if (n->balltype == 1)
	return;
    int p = n->position;
    if ( p < minnote)
	minnote = p;
    if ( p> maxnote)
	maxnote = p;
    heads.add(n);
    n->dependencies.add(this);
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
    } else
	stem_xoffset = 0;
}

void
Stem::set_noteheads()
{
    heads.sort(Notehead::compare);
    int parity=1;
    int lastpos = heads[0]->position;
    for (int i=1; i < heads.sz(); i ++) {
	int dy =abs(lastpos- heads[i]->position);
	
	if (dy <= 1) {
	    if (parity)
		heads[i]->x_dir = (stem_xoffset>0) ? 1:-1;
	    parity = !parity;
	} else
	    parity = 0;
	lastpos = heads[i]->position;
    }
}

void
Stem::do_pre_processing()
{
    if (bot == top)
	set_default_extents();
    set_noteheads();
}


Interval
Stem::width()const
{
    if (!print_flag || abs(flag) <= 4)
	return Interval(0,0);	// TODO!
    Paperdef*p= paper();
    Interval r(p->lookup_->flag(flag).dim.x);
    r+= stem_xoffset;
    return r;
}

Molecule*
Stem::brew_molecule()const return out;
{
    assert(pstaff_);
    assert(bot!=top);
 
    
    Paperdef *p =paper();

    Real dy = p->internote();
    Symbol ss =p->lookup_->stem(bot*dy,top*dy);

    
    out = new Molecule(Atom(ss));

    if (print_flag&&abs(flag) > 4){
	Symbol fl = p->lookup_->flag(flag);
	Molecule m(fl);
	if (flag < -4){		
	    out->add_bottom(m);
	} else if (flag > 4) {
	    out->add_top(m);
	} else
	    assert(false); 
    }

    out->translate(Offset(stem_xoffset,0));
}

Real
Stem::hpos()const
{
    return pcol_->hpos + stem_xoffset;
}


