/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "note-column.hh"
#include "script.hh"
#include "note-head.hh"
#include "stem.hh"


IMPLEMENT_IS_TYPE_B1(Note_column,Head_column);

Note_column::Note_column()
{
  h_shift_b_ =false;
}

void
Note_column::sort()
{
  head_l_arr_.sort (Note_head::compare);
}
  
Interval_t<int>
Note_column::head_positions_interval() const
{
  ((Note_column*)this)->sort();
  return Interval_t<int> (head_l_arr_[0]->position_i_, 
			     head_l_arr_.top()->position_i_);

}


void
Note_column::do_pre_processing()
{
  if (!dir_&& !stem_l_)
    {
	dir_ = (head_positions_interval().center () >=  5) ? DOWN:UP;
    }
  Head_column::do_pre_processing();
}

  
