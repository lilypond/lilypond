/*
  script-column.cc -- implement Script_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "script-column.hh"
#include "debug.hh"
#include "script.hh"
#include "note-head.hh"
#include "stem.hh"
#include "general-script-def.hh"

IMPLEMENT_IS_TYPE_B1(Script_column,Horizontal_vertical_group_item);


void
Script_column::add_script (Script*s_l)
{
  script_l_arr_.push (s_l);
  add_dependency (s_l);
  add_element (s_l);
}


void
Script_column::do_print() const
{
#ifndef NPRINT
  DOUT << "scripts: " << script_l_arr_.size() << '\n'; 
#endif
}

static int
idx (bool inside, int dir)
{
  int j = (dir+1);
  if (!inside)
    j ++;
  return j;
}

void
Script_column::do_pre_processing()
{
  if (!script_l_arr_.size()) 
    return;
  
  /* up+inside, up+outside, down+inside, down+outside */
  Array<Script*> placed_l_arr_a[4];
  for (int i=0; i < script_l_arr_.size(); i++) 
    {
      Script*s_l = script_l_arr_[i];
      placed_l_arr_a[idx (s_l->specs_p_->inside_b(),
			  s_l->dir_) ].push (s_l);
    }
  
  for (int j =0; j <4; j++) 
    {
      placed_l_arr_a[j].sort (Script::compare);
    }


  for (int j =0; j < 4; j++) 
    {
      if (placed_l_arr_a[j].size())
	for (int i=0; i  < support_l_arr_.size(); i++)
	  placed_l_arr_a[j][0]->add_support (support_l_arr_[i]);
    }
  
  Item * support_l=0;
  int j = 0;
  for (; j < 2; j++) 
    {
      for (int i=0; i < placed_l_arr_a[j].size(); i++) 
	{
	  if (support_l)
	    placed_l_arr_a[j][i]->add_support (support_l);
	  support_l = placed_l_arr_a[j][i];
	}
    }
  support_l = 0;
  for (; j < 4; j++) 
    {
      for (int i=0; i < placed_l_arr_a[j].size(); i++) 
	{
	  if (support_l)
	    placed_l_arr_a[j][i]->add_support (support_l);
	  support_l = placed_l_arr_a[j][i];
	}
    }
}


void
Script_column::add_support (Item*i_l)
{
  support_l_arr_.push (i_l);
  add_dependency (i_l);
  add_element (i_l);
}

void
Script_column::do_substitute_dependency (Score_element*o,Score_element*n)
{
  if (dynamic_cast <Item *> (o)) 
    {
      script_l_arr_.substitute ((Script*)dynamic_cast <Item *> (o),(Script*) (n?dynamic_cast <Item *> (n):0));
      support_l_arr_.substitute (dynamic_cast <Item *> (o), (n?dynamic_cast <Item *> (n):0));
    }
}
