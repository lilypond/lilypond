/*
  bar-column-grav.cc -- implement Bar_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "bar-column-engraver.hh"
#include "bar-column.hh"
#include "request.hh"
#include "script.hh"
#include "bar.hh"
#include <typeinfo>

Bar_column_engraver::Bar_column_engraver()
{
  bar_l_ =0;
  barcol_p_ =0;
  break_priority_i_ = 0;
}

void
Bar_column_engraver::do_creation_processing ()
{
}


void
Bar_column_engraver::do_process_requests ()
{
  Scalar pri = get_property ("barColumnPriority");
  if (pri.length_i() && pri.isnum_b ())
    {
      break_priority_i_ = int(pri);
    }
}

void
Bar_column_engraver::create_column ()
{
  if (!barcol_p_)
    {
      barcol_p_ = new Bar_column;
      barcol_p_->breakable_b_ =true;
      barcol_p_->break_priority_i_ = break_priority_i_;
      announce_element (Score_element_info (barcol_p_, 0));      
    }
}


void
Bar_column_engraver::acknowledge_element (Score_element_info info)
{
  Item * it = dynamic_cast <Item *> (info.elem_l_);
  if (!it)
    return;
  Script *s = dynamic_cast<Script*> (it);
  Bar *b = dynamic_cast<Bar*> (it);
  if (s
      && it->breakable_b_
      && info.origin_grav_l_arr_.size() == 1
      && it->break_priority_i_ == break_priority_i_)
    {
      create_column ();
      barcol_p_->add_script (s);
    }
  else if (info.origin_grav_l_arr_.size() == 1
	   && it->break_priority_i_ == break_priority_i_
	   && it->breakable_b_
	   &&b)

    {
      create_column ();
      barcol_p_->set_bar (b);
    }
}


void
Bar_column_engraver::process_acknowledged ()
{
}



void
Bar_column_engraver::do_pre_move_processing()
{
  if (barcol_p_) 
    {
      typeset_element (barcol_p_);
      barcol_p_ =0;
    }
}

void
Bar_column_engraver::do_post_move_processing()
{
  script_l_arr_.clear();
  bar_l_ =0;
}


ADD_THIS_TRANSLATOR(Bar_column_engraver);
