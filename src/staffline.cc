#include "staffline.hh"
#include "scoreline.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "symbol.hh"
#include "paper.hh"
#include "molecule.hh"
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
	((PStaff*)pstaff_)->
	    brew_molecule(line_of_score_->pscore_->paper_->linewidth);

	s+=pstaff_->stafsym->TeXstring();
	PCursor<const PCol *> cc(line_of_score_->cols);
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
    line_of_score_=sc;
    pstaff_=st;

    
    const PCol *linestart = sc->cols.top();
    const PCol *linestop = sc->cols.bottom();

    
    for (PCursor<const Spanner*> sp(pstaff_->spans); sp.ok(); sp++) {
	const PCol *brokenstart = &MAX(*linestart, *sp->left);
	const PCol *brokenstop = &MIN(*linestop, *sp->right);
	if ( *brokenstart < *brokenstop) {
	    line_of_score_->pscore_-> // higghl
		add_broken(sp->broken_at(brokenstart,brokenstop));
	}
    }
}


Interval
Line_of_staff::height() const
{
    Interval y;
    {
	y = pstaff_->stafsym->extent().y;
    }
    PCursor<const PCol *> cc(line_of_score_->cols);
    
    // all items in the current line & staff.
    for (; cc.ok(); cc++) {
	for (PCursor<const Item *> ic(cc->its); ic.ok(); ic++) {
	    if (ic->pstaff_ == pstaff_) {
		y.unite(ic->height());
	}
	    
	// spanners.
	for (PCursor<const Spanner *> sc(cc->starters); sc.ok(); sc++)
	    if (sc->pstaff_ == pstaff_) {
		y.unite(sc->height());
	    }
	}
    }
    return y;
}

void
Line_of_staff::process()
{
    if (!pstaff_->stafsym)
	pstaff_->brew_molecule(line_of_score_->pscore_->
			       paper_->linewidth);
}
