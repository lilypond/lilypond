/*
  head-column.cc -- implement Head_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "head-column.hh"
#include "note-head.hh"
#include "stem.hh"
#include "script.hh"

Head_column::Head_column()
{
  stem_l_ = 0;
  dir_i_ =0;
}

void
Head_column::do_substitute_dependency (Score_elem*o,
				      Score_elem*n)
{
  Script_column::do_substitute_dependency (o,n);
  if (o->name() == Note_head::static_name ()) 
    {
	head_l_arr_.substitute ((Note_head*)o->item(), 
				(n)? (Note_head*)n->item() : 0);
    }
  if (stem_l_ == o) 
    {
	stem_l_ = n ? (Stem*)n->item():0;
    }
}


void
Head_column::set (Stem*stem_l)
{
  stem_l_ = stem_l;
  Score_elem::add_dependency (stem_l);
  for (int i=0; script_l_arr_.size(); i++)
	script_l_arr_[i]->set_stem (stem_l);
}

void
Head_column::add (Script *script_l)
{
  Script_column::add (script_l) ;
  if  (stem_l_)
	script_l->set_stem (stem_l_);
}
void
Head_column::add (Note_head *n_l)
{
  add_support (n_l);
  head_l_arr_.push (n_l);
}



IMPLEMENT_IS_TYPE_B1(Head_column,Script_column);

void
Head_column::do_print() const
{
#ifndef NPRINT
  DOUT << "heads: " << head_l_arr_.size();
#endif
}

void
Head_column::do_pre_processing()
{
  if (!dir_i_)
    {
	if (stem_l_)
	    dir_i_ = stem_l_->dir_i_;
    }
  Script_column::do_pre_processing();
}
