/*
  note-column.cc -- implement Note_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "note-column.hh"
#include "debug.hh"
#include "script.hh"
#include "notehead.hh"
#include "stem.hh"

IMPLEMENT_STATIC_NAME(Note_column);


void
Note_column::add(Stem*stem_l)
{
    assert(!stem_l_);
    stem_l_ = stem_l;
    add_dependency(stem_l);
}

void
Note_column::add(Notehead* n_l)
{
    head_l_arr_.push(n_l);
    add_dependency(n_l);
}

void
Note_column::add(Script*s_l)
{
    script_l_arr_.push(s_l);
    add_dependency(s_l);
}

void
Note_column::translate(Offset o)
{
    for (int i=0; i < head_l_arr_.size(); i++)
	head_l_arr_[i]->translate(o);
    for (int i=0; i < script_l_arr_.size(); i++) 
	script_l_arr_[i]->translate(o);
    if (stem_l_)
	stem_l_->translate(o);
}


void
Note_column::do_print()const
{
    mtor << "heads: " << head_l_arr_.size() << '\n'; 
    mtor << "scripts: " << script_l_arr_.size() << '\n'; 
}

Interval
Note_column::do_height()const return r
{
    if (stem_l_)
	 r.unite(stem_l_->height());
    for (int i=0; i < head_l_arr_.size(); i++)
	r.unite(head_l_arr_[i]->height());
    for (int i=0; i < script_l_arr_.size(); i++) 
	r.unite(script_l_arr_[i]->height());
}

Interval
Note_column::do_width()const return r;
{
    if (stem_l_)
	 r.unite(stem_l_->width());
    for (int i=0; i < head_l_arr_.size(); i++)
	r.unite(head_l_arr_[i]->width());
    for (int i=0; i < script_l_arr_.size(); i++) 
	r.unite(script_l_arr_[i]->width());
}

void
Note_column::do_pre_processing()
{
    if (!script_l_arr_.size()) 
	return;

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
    
    Notehead *top_head_l=0;
    Notehead *bot_head_l=0;
    for (int i=0; i< head_l_arr_.size(); i++) {
	if (head_l_arr_[i]->extremal == -1)
	    bot_head_l = head_l_arr_[i];
	else if (head_l_arr_[i]->extremal == 1)
	    top_head_l = head_l_arr_[i];
    }
    /* argh. This sux. */
    if (!top_head_l) 
	top_head_l = bot_head_l;
    if (!bot_head_l) 
	bot_head_l = top_head_l;
    assert(bot_head_l && top_head_l);
    Item *support_l=top_head_l;
    int j;
    for (j = 0; j < 2; j++ ) {
	for (int i=0; i < placed_l_arr_a[j].size(); j++) {
	    placed_l_arr_a[j][i]->add_support(support_l);
	    support_l = placed_l_arr_a[j][i];
	}
    }
    
    support_l=bot_head_l;
    for (; j < 4; j++ ) {
	for (int i=0; i < placed_l_arr_a[j].size(); i++) {
	    placed_l_arr_a[j][i]->add_support(support_l);
	    support_l = placed_l_arr_a[j][i];
	}
    }
}
Note_column::Note_column()
{
    stem_l_ =0;
}
    
