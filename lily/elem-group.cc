/*
  elem-group.cc -- implement Element_group

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "elem-group.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"

Interval
Element_group::do_height()const 
{
    Interval r;
    for (int i=0; i < elem_l_arr_.size(); i++) 
	r.unite(elem_l_arr_[i]->height());
    return r;
}

Interval
Element_group::do_width()const 
{
    Interval r;
    for (int i=0; i < elem_l_arr_.size(); i++) 
	if (elem_l_arr_[i]->item()) // makes no at preprocessing for spanners. 
	    r.unite(elem_l_arr_[i]->width());
    return r;
}

void
Element_group::add_element(Score_elem*i_l)
{
    i_l->group_element_i_ ++;
    
    assert(! elem_l_arr_.find_l(i_l));
    elem_l_arr_.push(i_l);
    add_dependency(i_l);
}

void
Element_group::translate(Offset o)
{
    for (int i=0; i < elem_l_arr_.size(); i++) 
	elem_l_arr_[i]->translate(o);
}

IMPLEMENT_STATIC_NAME(Element_group);

void
Element_group::do_print() const
{
#ifndef NPRINT
    for (int i=0; i < elem_l_arr_.size(); i++) 
	mtor << elem_l_arr_[i]->name() << ' ';
#endif
}

void
Element_group::do_substitute_dependency(Score_elem* old, Score_elem *new_l)
{
    int i;

    while ((i=elem_l_arr_.find_i(old))>=0) {
	
	old->group_element_i_--;
	if (new_l){ 
	    new_l->group_element_i_ ++;
	    elem_l_arr_[i] = new_l;
	}else {	    
	    elem_l_arr_.del(i);
	}
    }

}

String
Element_group::TeX_string()const
{
    return "";
}

Element_group::Element_group(Element_group const&s)
    :elem_l_arr_(s.elem_l_arr_)
{
   for (int i=0; i < elem_l_arr_.size(); i++) 
	elem_l_arr_[i]->group_element_i_ ++;   
}

Element_group::Element_group()
{}
