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
Staff::truncate_cols(Moment l)
{
    iter_bot(cols, i);
    for (; i->when() > l; i=cols.bottom()) {
	i.del();
    }
}

Paperdef*
Staff::paper() const
{
    return score_l_->paper_p_;
}

void
Staff::clean_cols()
{
    iter_top(cols,i);
    for(; i.ok(); ){
	if (!i->score_column_l_->used())
	    i.del();
	else
	    i++;
    }
}

Staff_column *
Staff::get_col(Moment w, bool mus)
{
    Score_column* sc = score_l_->find_col(w,mus);
    
    iter_top(cols,i);
    for (; i.ok(); i++) {

	if (*i->score_column_l_ > *sc) // too far
	    break;
	if (sc == i->score_column_l_)
	    return i;
    }

    /* post: *sc > *->score_column_l_ || !i.ok() */
    Staff_column* newst = create_col(sc);

    if (!i.ok()) {
	cols.bottom().add(newst);
	return cols.bottom();
    }
    
    if (mus) {
	i.insert(newst);
	return newst;
    }

    
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
    set_time_descriptions();
}

void
Staff::set_time_descriptions()
{
    Time_description t(0,0);
    for (iter_top(cols,i); i.ok(); i++) {
	if (i->staff_commands_p_)
	    t = i->staff_commands_p_->tdescription_;
	else if (i->tdescription_)
	    t = *i->tdescription_;
	if(!i->tdescription_) {
	    i->tdescription_ = new Time_description(i->when() - t.when ,&t);
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
    assert(score_l_);
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
    mtor <<"}\n";
#endif
}

Staff::Staff()
{    
    score_l_ =0;
    pscore_l_ =0;    
}
