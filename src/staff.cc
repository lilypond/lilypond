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
    for (PCursor<Voice*> i(l); i.ok(); i++)
	voices.bottom().add(i);
}

void
Staff::process_commands(Real l)
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
    PCursor<Staff_column *> stc(cols);
    for(; stc.ok(); ){
	if (!stc->score_column->used())
	    stc.del();
	else
	    stc++;
    }
}

Staff_column *
Staff::get_col(Real w, bool mus)
{
    Score_column* sc = score_->find_col(w,mus);
    assert(sc->when == w);
    
    PCursor<Staff_column *> i(cols);
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
    for (PCursor<Voice*> i(voices); i.ok(); i++) {
	Real now = i->start;
	for (PCursor<Voice_element *> ve(i->elts); ve.ok(); ve++) {

	    Staff_column *sc=get_col(now,true);
	    sc->add(ve);
	    now += ve->duration;	    
	}	
    }

    for (PCursor<Command*> cc(*staff_commands_); cc.ok(); cc++) {
	Staff_column *sc=get_col(cc->when,false);
	sc->s_commands.add(cc);
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


Real
Staff::last() const
{
    Real l = 0.0;
    for (PCursor<Voice*> i(voices); i.ok(); i++) {
	l = MAX(l, i->last());
    }
    return l;
}


void
Staff::print() const
{
#ifndef NPRINT
    mtor << "Staff {\n";
    for (PCursor<Voice*> i(voices); i.ok(); i++) {
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
