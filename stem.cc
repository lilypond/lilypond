#include "stem.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "pstaff.hh"
#include "pscore.hh"
#include "paper.hh"
#include "lookupsyms.hh"
#include "molecule.hh"

const int STEMLEN=7;

Stem::Stem(int c)
{
    minnote = maxnote = 0;
    bot = top = 0;
    flag = 4;
    staff_center=c;
}

void
Stem::print()const
{
    mtor << "Stem minmax=["<< minnote<<","<<maxnote<<"], flag: "<<flag;
    Item::print();
}

void
Stem::calculate()
{
    assert(minnote<=maxnote);
    int stafftop = 2*staff_center;

    if (maxnote < -2){
	bot = minnote;
	top = staff_center - staff_center/2; // uhh... how about non 5-line staffs?
	
    }else if (minnote > stafftop + 2) {
	top = maxnote;
	bot = staff_center + staff_center/2;
	flag = -flag;
    }else {
	Real mean = (minnote+maxnote)/2;
	
	top = (mean > staff_center) ? maxnote : maxnote+STEMLEN;
	bot = (mean > staff_center) ? minnote-STEMLEN : minnote;
	flag = (mean > staff_center) ? -flag : flag;
    }
}

void
Stem::postprocess()
{
    calculate();
    brew_molecole();
}

Interval
Stem::width()const
{
    if (ABS(flag) <= 4)
	return Interval(0,0);	// TODO!
    Paperdef*p= pstaff_->pscore_->paper_;
    return p->lookup_->flag(flag).dim.x;
}

void
Stem::brew_molecole()
{
    assert(pstaff_);
    assert(bot!=top);
    assert(!output);
    
    Paperdef *p = pstaff_->pscore_->paper_;
    Parametric_symbol *stem = p->lookup_->stem();
    
    Real dy = p->interline()/2;
    String y1 =print_dimen( dy * bot);
    String y2 = print_dimen(dy * top);
    Symbol ss =stem->eval(y1,y2);
    delete stem;
    
    output = new Molecule(Atom(ss));

    if (ABS(flag) > 4){
	Symbol fl = p->lookup_->flag(flag);
	Molecule m(fl);
	if (flag < -4){		
	    output->add_bot(m);
	} else if (flag > 4) {
	    output->add_top(m);
	} else
	    assert(false); 
    }

    if (flag > 0){	
	Real dx = pstaff_->pscore_->paper_->note_width(); // ugh
	output->translate(Offset(dx,0));
    }
}
