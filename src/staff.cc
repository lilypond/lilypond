#include "staff.hh"
#include "score.hh"
#include "voice.hh"
#include "swalker.hh"
#include "stcol.hh"
#include "sccol.hh"
#include "staffcommands.hh"
#include "debug.hh"

void
Staff::add(PointerList<Voice*> &l)
{
    for (iter_top(l,i); i.ok(); i++)
	voices.bottom().add(i);
}

void
Staff::process_commands(Moment l)
{
    if (staff_commands_)
	staff_commands_->clean(l);
}

Paperdef*
Staff::paper() const
{
    return score_->paper_;
}

void
Staff::clean_cols()
{
    iter_top(cols,i);
    for(; i.ok(); ){
	if (!i->score_column->used())
	    i.del();
	else
	    i++;
    }
}

Staff_column *
Staff::get_col(Moment w, bool mus)
{
    Score_column* sc = score_->find_col(w,mus);
    assert(sc->when == w);
    
    iter_top(cols,i);
    for (; i.ok(); i++) {
	if (*i->score_column > *sc) // too far
	    break;
	if (sc == i->score_column)
	    return i;
    }

    /* post: *sc > *->score_column || !i.ok() */
    Staff_column* newst = create_col(sc);

    if (!i.ok()) {
	cols.bottom().add(newst);
	return cols.bottom();
    }
    
    if (mus) {
	i.insert(newst);
	return newst;
    }

//  ;  assert((i-1).ok())
    // todo!
    
    // making a fix at 2:30 am, with several beers drunk.
    // but it works :-)
    if ((i-1).ok()&& (i-1)->when() == newst->when()) {
	i--;
    }

    i.insert(newst);
    
    return newst;
}



/*
    put all stuff grouped vertically in the Staff_cols
    */
void
Staff::setup_staffcols()
{    
    for (iter_top(voices,i); i.ok(); i++) {
	Moment now = i->start;
	for (iter_top(i->elts,ve); ve.ok(); ve++) {

	    Staff_column *sc=get_col(now,true);
	    sc->add(ve);
	    now += ve->duration;	    
	}	
    }

    for (iter_top(*staff_commands_,cc); cc.ok(); cc++) {
	Staff_column *sc=get_col(cc->tdescription_.when,false);
	sc->s_commands = cc;
	sc->tdescription_ = new Time_description(cc->tdescription_);
    }

    iter_top(*staff_commands_,cc);
    for (iter_top(cols,i); i.ok(); i++) {
	while  ((cc+1).ok() && (cc+1)->when() < i->when())
	    cc++;

	if(!i->tdescription_) {
	    if (cc->tdescription_.when == i->when())
		i->tdescription_ = new Time_description(cc->tdescription_);
	    else
		i->tdescription_ = new Time_description(
		    i->when() - cc->when() ,&cc->tdescription_);
	}
    }
}

void
Staff::process()
{
    setup_staffcols();
    OK();
    walk();
}

void
Staff::OK() const
{
#ifndef NDEBUG
    cols.OK();
    voices.OK();
    assert(score_);    
#endif    
}


Moment
Staff::last() const
{
    Moment l = 0.0;
    for (iter_top(voices,i); i.ok(); i++) {
	l = l >? i->last();
    }
    return l;
}


void
Staff::print() const
{
#ifndef NPRINT
    mtor << "Staff {\n";
    for (iter_top(voices,i); i.ok(); i++) {
	i->print();	
    }
    if (staff_commands_)
	staff_commands_->print();
    mtor <<"}\n";
#endif
}

Staff::Staff()
{    
    staff_commands_ = 0;
    score_ =0;
    pscore_=0;    
}
