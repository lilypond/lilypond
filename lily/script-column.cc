/*
  script-column.cc -- implement Script_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "script-column.hh"
#include "debug.hh"
#include "script.hh"
#include "notehead.hh"
#include "stem.hh"

IMPLEMENT_STATIC_NAME(Script_column);


void
Script_column::add(Script*s_l)
{
    script_l_arr_.push(s_l);
    add_dependency(s_l);
}

void
Script_column::translate(Offset o)
{
    for (int i=0; i < script_l_arr_.size(); i++) 
	script_l_arr_[i]->translate(o);
    for (int i=0; i < support_l_arr_.size(); i++)
    	support_l_arr_[i]->translate(o);
}


void
Script_column::do_print()const
{
    mtor << "scripts: " << script_l_arr_.size() << '\n'; 
}

Interval
Script_column::do_height()const return r
{
    for (int i=0; i < script_l_arr_.size(); i++) 
	r.unite(script_l_arr_[i]->height());
}

Interval
Script_column::do_width()const 
{
    Interval r;
    for (int i=0; i < script_l_arr_.size(); i++) 
	r.unite(script_l_arr_[i]->width());
    return r;
}

void
Script_column::do_pre_processing()
{
    if (!script_l_arr_.size()) 
	return;
    /* up+outside, up+inside, down+outside, down+inside */
    Array<Script*> placed_l_arr_a[4];
    for (int i=0; i < script_l_arr_.size(); i++) {
	Script*s_l = script_l_arr_[i];
	int j = (s_l->dir_i_ >0) ? 0 : 2;
	if (!s_l->inside_staff_b_) 
	    j ++;
	
	placed_l_arr_a[j].push(s_l);
    }
    
    for (int j =0; j <4; j++) {
	placed_l_arr_a[j].sort( Script::compare);
    }


    for (int j =0; j < 4; j++) {
	if (placed_l_arr_a[j].size())
	    for (int i=0; i  < support_l_arr_.size(); i++)
		placed_l_arr_a[j][0]->add_support( support_l_arr_[i]);
    }
    Item * support_l=0;
    int j = 0;
    for (; j < 2; j++ ) {
	for (int i=1; i < placed_l_arr_a[j].size(); i++) {
	    if (support_l)
		placed_l_arr_a[j][i]->add_support(support_l);
	    support_l = placed_l_arr_a[j][i];
	}
    }
    support_l = 0;
    for (; j < 4; j++ ) {
	for (int i=1; i < placed_l_arr_a[j].size(); i++) {
	    if (support_l)
		placed_l_arr_a[j][i]->add_support(support_l);
	    support_l = placed_l_arr_a[j][i];
	}
    }
}


void
Script_column::add_support(Item*i_l)
{
    support_l_arr_.push(i_l);
    add_dependency(i_l);
}
