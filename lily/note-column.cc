/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "dot-column.hh"
#include "note-column.hh"
#include "script.hh"
#include "note-head.hh"
#include "stem.hh"
#include "rest.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1(Note_column,Script_column);

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

Interval 
Note_column::width () const
{
  if (head_l_arr_.size ())
    return head_l_arr_[0]->width ();
  else
    return Interval (0,0);
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
	  dir_ = (head_positions_interval().center () >=  5) ? DOWN:UP;
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
Note_column::do_substitute_dependency (Score_element*o, Score_element*n)
{
  if (stem_l_ == o) 
    {
      stem_l_ = n ? (Stem*)n->access_Item ():0;
    }
  if (o->is_type_b (Note_head::static_name ()))
    {
      head_l_arr_.substitute ((Note_head*)o->access_Item (), 
			      (n)? (Note_head*)n->access_Item () : 0);
    }
  Script_column::do_substitute_dependency (o,n);
  if (o->is_type_b (Rest::static_name ())) 
    {
      rest_l_arr_.substitute ((Rest*)o->access_Item (), 
			      (n)? (Rest*)n->access_Item () : 0);
    }
}

void
Note_column::add_head (Rhythmic_head *h)
{
  if (h->is_type_b (Rest::static_name ()))
    {
      rest_l_arr_.push ((Rest*)h);
      add_support (h);  
    }
  if (h->is_type_b (Note_head::static_name ()))
    {
      head_l_arr_.push ((Note_head*) h);
      add_support (h);
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
