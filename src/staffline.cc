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
    Real r = (i.empty()) ? 0.0 : i.length();
    String s("\\vbox to ");
    s += print_dimen(r);
    s += "{\\vskip "+print_dimen(i.right)+" ";
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
	iter_top(line_of_score_l_->cols,cc);
	Real lastpos=cc->hpos;

	// all items in the current line & staff.
	for (; cc.ok(); cc++) {
	    Real delta=cc->hpos - lastpos;
	    lastpos = cc->hpos;

	    // moveover
	    if (delta)
		s +=String( "\\kern ") + print_dimen(delta);

	    // now output the items.
	    for (iter_top(cc->its,i); i.ok(); i++) {
		if (i->pstaff_l_ == pstaff_l_)
		    s += i->TeXstring();
	    }
	    // spanners.
	    for (iter_top(cc->starters,i); i.ok(); i++)
		if (i->pstaff_l_ == pstaff_l_)
		    s += i->TeXstring();
	}
    }
    s+="\\hss}\\vss}";
    return s;
}

Line_of_staff::Line_of_staff(Line_of_score * sc, PStaff*st)
{
    line_of_score_l_=sc;
    pstaff_l_=st;

    PCol *linestart = sc->cols.top();
    PCol *linestop = sc->cols.bottom();
    
    for (iter_top(pstaff_l_->spans,i); i.ok(); i++) {
	PCol *brokenstart = &max(*linestart, *i->left);
	PCol *brokenstop = &min(*linestop, *i->right);
	if ( *brokenstart < *brokenstop) {
	    Spanner*span_p =i->broken_at(brokenstart,brokenstop);
	    line_of_score_l_->pscore_l_-> // higghl
		add_broken(span_p);
	}
    }
}


Interval
Line_of_staff::height() const
{
    Interval y(0,0);

    iter_top(line_of_score_l_->cols,cc);
    
    // all items in the current line & staff.
    for (; cc.ok(); cc++) {
	for (iter_top(cc->its,i); i.ok(); i++) {
	    if (i->pstaff_l_ == pstaff_l_) 
		y.unite(i->height());
	    
	}
	// spanners.
	for (iter_top(cc->starters,i); i.ok(); i++)
	    if (i->pstaff_l_ == pstaff_l_) {
		y.unite(i->height());
	    }
    }
    
    return y;
}

void
Line_of_staff::process()
{
#if 0
    if (!pstaff_l_->stafsym_p_)
	pstaff_l_->brew_molecule_p(line_of_score_l_->pscore_l_->
				 paper_l_->linewidth);
#endif
}
