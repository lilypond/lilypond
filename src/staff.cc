#include "staff.hh"
#include "score.hh"
#include "voice.hh"
#include "staff-walker.hh"
#include "staff-column.hh"
#include "score-column.hh"
#include "voice-element.hh"
#include "debug.hh"
#include "musicalrequest.hh"
#include "commandrequest.hh" // todo
#include "midi-stream.hh"

void
Staff::add(PointerList<Voice*> const &l)
{
    for (iter_top(l,i); i.ok(); i++)
	voice_list_.bottom().add(i);
}

Paper_def *
Staff::paper() const
{
    return score_l_->paper_p_;
}

void
Staff::clean_cols()
{
    iter_top(cols_,i);
    for(; i.ok(); ){
	if (!i->musical_column_l_->used_b())
	    i->musical_column_l_ = 0;
	if (!i->command_column_l_->used_b())
	    i->command_column_l_ =0;
	
	if (!i->command_column_l_&& !i->musical_column_l_)
	    delete i.get_p();
	else
	    i++;
    }
}

Staff_column *
Staff::get_col(Moment w, PCursor<Staff_column*> *last)
{    
    iter_top(cols_,i);
    if (last && last->ok() && (*last)->when() <= w)
	i = *last;
    
    for (; i.ok(); i++) {
	if (i->when() == w) {
	    if (last)
		*last = i;
	    return i;
	} else if (i->when() > w)
	    break;
    }


    PCursor<Score_column*> scorecolumns(score_l_->find_col(w, false));
    Staff_column* staffcolumn_p = new Staff_column;
    staffcolumn_p->staff_l_ = this;
    Score_column* comcol_l  = scorecolumns++;
    staffcolumn_p->set_cols(comcol_l, scorecolumns);
    
    if (!i.ok()) {
	cols_.bottom().add(    staffcolumn_p);
	i = cols_.bottom();
    } else {
	i.insert(staffcolumn_p);
	i--;
    }
    if (last)
	*last = i;
    return i;
}

/**
  put all stuff grouped vertically in the Staff_cols.
  Do the preprarations for walking the cols. not virtual
    */
void
Staff::setup_staffcols()
{    
    for (iter_top(voice_list_,i); i.ok(); i++) {
	PCursor<Staff_column*> last(cols_);
	Moment now = i->start;
	for (iter_top(i->elts,j); j.ok(); j++) {
	    
	    Staff_column *s_l= get_col(now, &last);
	    assert(now == s_l->when());
	    s_l->add(j);
	    now += j->duration;	    
	}

    }
    OK();
}

void
Staff::OK() const
{
#ifndef NDEBUG
    cols_.OK();
    voice_list_.OK();
    iter_top(cols_, i);
    iter_top(cols_, j);
    i++;
    for (; i.ok(); j++,i++) {
	assert(j->when () < i->when() );
    }
    assert(score_l_);
#endif    
}


Moment
Staff::last() const
{
    Moment l = 0;
    for (iter_top(voice_list_,i); i.ok(); i++) {
	l = l >? i->last();
    }
    return l;
}

void
Staff::print() const
{
#ifndef NPRINT
    mtor << "Staff {\n";
    for (iter_top(voice_list_,i); i.ok(); i++) {
	i->print();	
    }
    mtor <<"}\n";
#endif
}

Staff::Staff()
{    
    score_l_ =0;
    pscore_l_ =0;
    pstaff_l_ =0;
}
