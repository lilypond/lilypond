#include "line.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "symbol.hh"
#include "paper.hh"
#include "pcol.hh"
#include "pscore.hh"

static String
make_vbox(Interval i)
{
    
    String s("\\vbox to ");
    s += print_dimen(i.length());
    s += "{\\vskip "+print_dimen(i.max)+" ";
    return s;
}

    
String
Line_of_staff::TeXstring() const
{
    String s("%line_of_staff\n");
    s+=make_vbox(height());
    // the staff itself: eg lines, accolades
    s += "\\hbox{";
    {
	Symbol sym = pstaff_->get_stafsym(scor->score->paper_->linewidth);
	s+=sym.tex;
	PCursor<const PCol *> cc(scor->cols);
	Real lastpos=cc->hpos;

	// all items in the current line & staff.
	for (; cc.ok(); cc++) {
	    Real delta=cc->hpos - lastpos;
	    lastpos = cc->hpos;

	    // moveover
	    if (delta)
		s +=String( "\\kern ") + print_dimen(delta);

	    // now output the items.

	    for (PCursor<const Item *> ic(cc->its); ic.ok(); ic++) {
		if (ic->pstaff_ == pstaff_)
		    s += ic->TeXstring();
	    }
	    // spanners.
	    for (PCursor<const Spanner *> sc(cc->starters); sc.ok(); sc++)
		if (sc->pstaff_ == pstaff_)
		    s += sc->TeXstring();
	}
    }
    s+="\\hss}\\vss}";
    return s;
}

Line_of_staff::Line_of_staff(Line_of_score * sc, PStaff*st)
{
 
    scor=sc;
    pstaff_=st;

    const    PCol *linestart= sc->cols.top();
    const PCol *linestop=sc->cols.bottom();
    
    for (PCursor<const Spanner*> sp(pstaff_->spans); sp.ok(); sp++) {
	const PCol *brokenstart = &MAX(*linestart, *sp->left);
	const PCol *brokenstop = &MIN(*linestop, *sp->right);

	if (*brokenstop  < *brokenstart)
	    brokenspans.bottom().add(sp->broken_at(brokenstop, brokenstart));
    }
}


Interval
Line_of_staff::height() const
{
    Interval y;
    {
	Symbol s = pstaff_->stafsym->eval(scor->score->paper_->linewidth);
	y = s.dim.y;
    }
    PCursor<const PCol *> cc(scor->cols);
    
    // all items in the current line & staff.
    for (; cc.ok(); cc++) {
	for (PCursor<const Item *> ic(cc->its); ic.ok(); ic++) {
	    if (ic->pstaff_ == pstaff_) {
		y.unite(ic->height());
	}
	    
	// spanners.
	for (PCursor<const Spanner *> sc(cc->starters); sc.ok(); sc++)
	    if (sc->pstaff_ == pstaff_)
		assert(false);		
	}
    }
    return y;
}


/****************************************************************/
String
Line_of_score::TeXstring() const
{
     String s("\\vbox{%<- line of score\n");
     for (PCursor<Line_of_staff*> sc(staffs); sc.ok(); sc++){
	 s += sc->TeXstring();
	 if ((sc+1).ok())
	     s+= "\\interstaffline\n";
     }
     s += "}";
     return s;
}

/// testing this entry
Line_of_score::Line_of_score(svec<const PCol *> sv,
			     const PScore *ps)
{
    score = ps;
    for (int i=0; i< sv.sz(); i++) {
	PCol *p=(PCol *) sv[i];
	cols.bottom().add(p);
	p->line=this;
    }

    for (PCursor<PStaff*> sc(score->staffs); sc.ok(); sc++)
	staffs.bottom().add(new Line_of_staff(this, sc));    
}
/** construct a line with the named columns. Make the line field
    in each column point to this
    
    #sv# isn't really const!!
    */

