#include "stem.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper-def.hh"
#include "notehead.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "misc.hh"

const int STEMLEN=7;


Stem::Stem(int c) //, Moment len)
{
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

IMPLEMENT_STATIC_NAME(Stem);

void
Stem::do_print() const
{
#ifndef NPRINT
    mtor << "flag "<< flag << " print_flag " << print_flag
	 << "min,max [" << minnote << ", " << maxnote << "]";
#endif
}
void
Stem::set_stemend(Real se)
{

    // todo: margins
    if (!  ((dir > 0 && se >= maxnote) || (se <= minnote && dir <0))  )	
 	warning("Weird stem size; check for narrow beams");
    
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
    if (p < minnote)
	minnote = p;
    if (p > maxnote)
	maxnote = p;
    heads.push(n);
    n->add_dependency(this);
}


int
Stem::get_default_dir()
{
    if (dir)
	return dir;
    Real mean = (minnote+maxnote)/2;
    return (mean > staff_center) ? -1: 1;
}

void
Stem::set_default_dir()
{
    dir = get_default_dir();
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
    if (minnote > maxnote) {
	warning("Empty stem. Ugh!");
	minnote = -10;
	maxnote = 20;
    }

    if (!stemlen)
	set_default_stemlen();

    set_stemend((dir< 0) ? maxnote-stemlen: minnote +stemlen);
    if (dir > 0){	
	stem_xoffset = paper()->note_width()-paper()->rule_thickness();
    } else
	stem_xoffset = 0;
}

void
Stem::set_noteheads()
{
    if(!heads.size())
	return;
    heads.sort(Notehead::compare);
    heads[0]->extremal = -1;
    heads.top()->extremal = 1;
    int parity=1;
    int lastpos = heads[0]->position;
    for (int i=1; i < heads.size(); i ++) {
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
    Paper_def*p= paper();
    Interval r(p->lookup_l()->flag(flag).dim.x);
    r+= stem_xoffset;
    return r;
}

Molecule*
Stem::brew_molecule_p()const return out;
{
    assert(bot!=top);
 
    
    Paper_def *p =paper();

    Real dy = p->internote();
    Symbol ss =p->lookup_l()->stem(bot*dy,top*dy);

    
    out = new Molecule(Atom(ss));

    if (print_flag&&abs(flag) > 4){
	Symbol fl = p->lookup_l()->flag(flag);
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
Stem::hindex()const
{
    return pcol_l_->hpos + stem_xoffset; // hmm.  + offset_.x;
}


