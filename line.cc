#include "line.hh"
#include "symbol.hh"
#include "cols.hh"
#include "pscore.hh"

String
Line_of_staff::TeXstring() const
{
    String s("%line_of_staff\n\\vbox to ");
    s += String(maxheight() * VERT_TO_PT) +"pt{";

    //make some room
    s += vstrut(base* VERT_TO_PT);

    // the staff itself: eg lines, accolades
    s += "\\hbox{";
    {
	s+=(*pstaff_->stafsym)(scor->score->linewidth);
	PCursor<const PCol *> cc(scor->cols);
	Real lastpos=cc->hpos;

	// all items in the current line & staff.
	for (; cc.ok(); cc++) {
	    Real delta=cc->hpos - lastpos;
	    lastpos = cc->hpos;

	    // moveover
	    s +=String( "\\kern ") + HOR_TO_PT*delta + "pt ";

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
    s+="\\hss}}";
    return s;
}

String
Line_of_score::TeXstring() const
{
     String s("\\vbox{");
     for (PCursor<Line_of_staff*> sc(staffs); sc.ok(); sc++)
	 s += sc->TeXstring();
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

Line_of_staff::Line_of_staff(Line_of_score * sc, PStaff*st)
{
    // [don't know how to calc dimensions yet.]
   height = 0.0;
   base =0.0;
 
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


Real
Line_of_staff::maxheight() const
{
    Interval y;
    y = pstaff_->stafsym->height(scor->score->linewidth);
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
    return y.max;
}


