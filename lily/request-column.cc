/*
  request-column.cc -- implement Request_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "score-column.hh"
#include "request-column.hh"
#include "staff-column.hh"
#include "staff.hh"

Moment
Request_column::when()
{
    if (command_column_l_ || musical_column_l_)
	when_ = (command_column_l_)? command_column_l_->when() 
	    : musical_column_l_->when();

    return when_;
}

void
Request_column::add_reqs(int idx , Array<Request*> const & req_l_arr)
{
    staff_col_l_arr_[idx]->add_reqs(req_l_arr);
}

Request_column::Request_column(Link_list<Staff*> const& list )
{
    musical_column_l_ = command_column_l_ =0;
    iter(list.top(), j);
    for (int i=0; i < list.size(); i++,j++) {
	Staff_column * col_p = new Staff_column;
	col_p->set_req_col(this);
	staff_col_l_arr_.push(col_p);
	staff_cols_.bottom().add(col_p);
	j->add_col(col_p);
    }
    when_ = 0;
}

void
Request_column::set_score_cols(Score_column* c1, Score_column *c2)
{
    command_column_l_ = c1;
    musical_column_l_ = c2;
}
bool
Request_column::used_b() const
{
    bool b = false;
    if (command_column_l_)
	b |= command_column_l_->used_b();
    if (musical_column_l_)
	b |= command_column_l_->used_b();
    return b;
}
void
Request_column::update_time(int idx, Time_description&t)
{
    staff_col_l_arr_[idx]->update_time(t, 0);
}
