#include "staff.hh"
#include "stcol.hh"
#include "sccol.hh"
#include "debug.hh"

Staff::Staff(Staff const&src)
{
    PL_copy(voices,src.voices);
    PL_copy(commands,src.commands);
    assert(!cols.size());	// cols is a runtime field.

    score_ = src.score_;
    pscore_ = src.pscore_;
}

Paperdef*
Staff::paper() const{
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
    PCursor<Staff_column *> stc(cols);
    for (; stc.ok(); stc++) {
	if (*stc->score_column > *sc) // too far
	    break;
	if (sc == stc->score_column)
	    return stc;
    }
    Staff_column* newst = create_col(sc);

    if (!stc.ok()) {
	cols.bottom().add(newst);
	return cols.bottom();
    }
    
    if (mus) {
	stc.insert(newst);
	return newst;
    }

    if ((stc-1)->when() == newst->when()) {
	stc--;
    }

    stc.insert(newst);
    
    return newst;
}


void
Staff::add_voice(Voice *v)
{
    voices.bottom().add(v);
}

/*
    put all stuff grouped vertically in the Staff_cols
    */
void
Staff::setup_staffcols()
{
    
    for (PCursor<Voice*> vc(voices); vc.ok(); vc++) {

	Real now = vc->start;
	for (PCursor<Voice_element *> ve(vc->elts); ve.ok(); ve++) {

	    Staff_column *sc=get_col(now,true);
	    sc->add(ve);
	    now += ve->duration;	    
	}	
    }

    for (PCursor<Command*> cc(commands); cc.ok(); cc++) {
	Staff_column *sc=get_col(cc->when,false);
	sc->s_commands.add(cc);
    }
}

/// merge commands from score
void
Staff::add_commands(PointerList<Command*> const &cl)
{
    PCursor<Command*> score_c(cl);
    PCursor<Command*> cc(commands);
    
    while (score_c.ok()) {
	while (cc.ok() && cc->when <= score_c->when)
	    cc++;
	
	Command*nc = new Command (*(* score_c));
	if (cc.ok()) {
	    // cc->when > score_c->when
	    cc.insert( nc );
	} else {
	    commands.bottom().add( nc);
	    cc = commands.bottom();
	}
	score_c++;
    }

    // now integrate break commands with other commands.
    // maybe do this in derived functions.
}

void
Staff::process()
{
    setup_staffcols();
    OK();
    for (PCursor<Staff_column*> sc(cols); sc.ok(); sc++) {
	sc->process_commands();
	sc->process_requests();
    }
    grant_requests();
}

void
Staff::OK() const
{
#ifndef NDEBUG
    cols.OK();
    commands.OK();
    voices.OK();
    assert(score_);    
#endif    
}


Real
Staff::last() const
{
    Real l = 0.0;
    for (PCursor<Voice*> vc(voices); vc.ok(); vc++) {
	l = MAX(l, vc->last());
    }
    return l;
}


void
Staff::print() const
{
#ifndef NPRINT
    mtor << "Staff {\n";
    for (PCursor<Voice*> vc(voices); vc.ok(); vc++) {
	vc->print();
	
    }
    mtor <<"}\n";
#endif
}

Staff::Staff()
{
    score_ =0;
    pscore_=0;    
}
