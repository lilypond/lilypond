/*
  scoreline.cc -- implement Line_of_score

  source file of the LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "scoreline.hh"
#include "staffline.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "symbol.hh"
#include "paper-def.hh"
#include "p-col.hh"
#include "p-score.hh"


String
Line_of_score::TeXstring() const
{
     String s("\\vbox{%<- line of score\n");
     if (error_mark_b_)
	 s+= "\\scorelineerrormark";
     for (iter_top(staffs,sc); sc.ok(); sc++){
	 s += sc->TeXstring();
	 if ((sc+1).ok())
	     s+= "\\interstaffline\n";
     }
     s += "}";
     return s;
}


Line_of_score::Line_of_score(Array<PCol *> sv,
			     PScore *ps)
{
    error_mark_b_ = 0;
    pscore_l_ = ps;
    for (int i=0; i< sv.size(); i++) {
	PCol *p=(PCol *) sv[i];
	cols.bottom().add(p);
	p->line_l_=this;
    }

    for (iter_top(pscore_l_->staffs,sc); sc.ok(); sc++)
	staffs.bottom().add(new Line_of_staff(this, sc));    
}
/* construct a line with the named columns. Make the line field
    in each column point to this
    
    #sv# isn't really const!!
    */


void
Line_of_score::process()
{
    for (iter_top(staffs,i); i.ok(); i++)
	i->process();
}
	
