#include "staff.hh"
#include "score.hh"
#include "voice.hh"
#include "swalker.hh"
#include "stcol.hh"
#include "sccol.hh"
#include "staffcommands.hh"
#include "debug.hh"
#include "inputcommands.hh"
#include "inputcommand.hh"
#include "request.hh"

void
Staff::do_commands(PointerList<Input_command*> score_wide,
		   PointerList<Input_command*> staff_wide)
{
    Input_commands commands;
    for (iter_top(score_wide,i); i.ok(); i++) 
	commands.add(**i, score_l_->markers_assoc_);
    for (iter_top(staff_wide,i); i.ok(); i++) 
	commands.add(**i,score_l_->markers_assoc_);

    commands.parse(this);
}

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
	Staff_column * col_p = i.get();
	assert(col_p->when() > l);
	delete col_p;
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
	    delete i.get();
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

void
Staff::get_marks(Array<String>&s_arr, Array<Moment>&m_arr)
{
     for (iter_top(voices,i); i.ok(); i++) {
	Moment now = i->start;
	for (iter_top(i->elts,j); j.ok(); j++) {
	    for (iter_top(j->reqs, k); k.ok(); k++) {
		if (k->mark()) { // ugh. 4 levels
		    s_arr.add(k->mark()->mark_str_);
		    m_arr.add(now);
		}
	    }
	    now += j->duration;	    
	}
     }
}
/*
    put all stuff grouped vertically in the Staff_cols
    */
void
Staff::setup_staffcols()
{    
    for (iter_top(voices,i); i.ok(); i++) {
	Moment now = i->start;
	for (iter_top(i->elts,j); j.ok(); j++) {

	    Staff_column *s_l=get_col(now,true);
	    s_l->add(j);
	    now += j->duration;	    
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
