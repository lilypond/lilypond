#include "staff.hh"
#include "debug.hh"
#include "pscore.hh"

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

/*
    maak een staff column, met specs in args.

    (sorry wat is het vroeg vandaag..)
    */
Staff_column *
Staff::get_col(Mtime w, bool mus)
{
    Score_column* sc = score_->find_col(w,mus);
    assert(sc->when == w);
    PCursor<Staff_column *> stc(cols);
    for (; stc.ok(); stc++) {
	if (*sc  < *stc->score_column)
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

	Mtime now = vc->start;
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
    // may be do this in derived functions.
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


Mtime
Staff::last() const {
    Mtime l = 0.0;
    for (PCursor<Voice*> vc(voices); vc.ok(); vc++) {
	l = MAX(l, vc->last());
    }
    return l;
}


void
Staff::print() const
{
    mtor << "Staff {\n";
    for (PCursor<Voice*> vc(voices); vc.ok(); vc++) {
	vc->print();
	
    }
    mtor <<"}\n";
}

/****************************************************************/

bool
Staff_column::mus() const
{
    return score_column->musical;
}

Mtime
Staff_column::when() const
{
    return score_column->when;
}

void
Staff_column::add(Voice_element*ve)
{
    Mtime d= ve->duration;
    if (d){
	score_column->durations.add(d);
    }
	
    v_elts.add(ve);
}

Staff_column::Staff_column(Score_column*s) {
    score_column = s;
}
