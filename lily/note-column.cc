/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "dot-column.hh"
#include "note-column.hh"
#include "script.hh"
#include "note-head.hh"
#include "stem.hh"
#include "rest.hh"
#include "debug.hh"

bool
Note_column::rest_b () const
{
  return rest_l_arr_.size ();
}

Note_column::Note_column()
{
  stem_l_ = 0;
  h_shift_b_ = false;
  dir_ =CENTER;
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
  Interval_t<int>  iv;

  iv.set_empty ();

  if (head_l_arr_.size ())
    iv = Interval_t<int>(head_l_arr_[0]->position_i_, 
			 head_l_arr_.top()->position_i_);
  
  return iv;
}

void
Note_column::do_pre_processing()
{
  if (!dir_)
    {
      if (stem_l_)
	dir_ = stem_l_->dir_;
      else if (head_l_arr_.size ())
	{
	  //	  assert (false);	// looks obsolete?
	  dir_ = sign (head_positions_interval().center ());
	}
    }
  Script_column::do_pre_processing();
}

  

void
Note_column::set_stem (Stem * stem_l)
{
  add_support (stem_l);
  stem_l_ = stem_l;
  /* 
     don't add stem to support; mostly invisible for rest-columns (and possibly taken . .)
  */
  Score_element::add_dependency (stem_l);
  for (int i=0; i < script_l_arr_.size(); i++)
    script_l_arr_[i]->set_stem (stem_l);
}

void
Note_column::add_script (Script *script_l)
{
  Script_column::add_script (script_l) ;
  if  (stem_l_)
    script_l->set_stem (stem_l_);
}

void
Note_column::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  if (stem_l_ == o) 
    {
      stem_l_ = n ? dynamic_cast<Stem *> (n):0;
    }
  if (dynamic_cast<Note_head *> (o))
    {
      head_l_arr_.substitute (dynamic_cast<Note_head *> (o), 
			      (n)? dynamic_cast<Note_head *> (n) : 0);
    }
  Script_column::do_substitute_element_pointer (o,n);
  if (dynamic_cast<Rest *> (o)) 
    {
      rest_l_arr_.substitute (dynamic_cast<Rest *> (o), 
			      (n)? dynamic_cast<Rest *> (n) : 0);
    }
}

void
Note_column::add_head (Rhythmic_head *h)
{
  if (Rest*r=dynamic_cast<Rest *> (h))
    {
      rest_l_arr_.push (r);
      add_support (r);  
    }
  if (Note_head *nh=dynamic_cast<Note_head *> (h))
    {
      head_l_arr_.push (nh);
      add_support (nh);
    }
}

/**
  translate the rest symbols
 */
void
Note_column::translate_rests (int dy_i)
{
  invalidate_cache (Y_AXIS);
  for (int i=0; i < rest_l_arr_.size(); i++)
    rest_l_arr_[i]->position_i_ += dy_i;
}

void
Note_column::do_print() const
{
#ifndef NPRINT
  DOUT << "rests: " << rest_l_arr_.size() << ", ";
  DOUT << "heads: " << head_l_arr_.size();
#endif
}

void
Note_column::set_dotcol (Dot_column *d)
{
  add_element (d);
}
