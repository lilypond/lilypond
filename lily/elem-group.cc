/*
 elem-group.cc -- implement Horizontal_vertical_group

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "elem-group.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"

bool
Elbement_group::contains_b(Score_elem const*e)const
{
    return elem_l_arr_.find_l(e);
}

Interval
Vertical_group::do_height()const 
{
    Interval r;
    for (int i=0; i < elem_l_arr_.size(); i++) 
	r.unite(elem_l_arr_[i]->height());
    return r;
}

Interval
Horizontal_group::do_width()const 
{
    Interval r;
    for (int i=0; i < elem_l_arr_.size(); i++) 
	if (elem_l_arr_[i]->item()) // makes no at preprocessing for spanners. 
	    r.unite(elem_l_arr_[i]->width());
    return r;
}

IMPLEMENT_STATIC_NAME(Horizontal_group);
IMPLEMENT_IS_TYPE_B1(Horizontal_group, Elbement_group);
IMPLEMENT_IS_TYPE_B1(Vertical_group, Elbement_group);
IMPLEMENT_STATIC_NAME(Vertical_group);

void
Elbement_group::add_element(Score_elem*i_l)
{
    assert(i_l!= this);
    assert(! contains_b(i_l));

    elem_l_arr_.push(i_l);
    add_dependency(i_l);
}

void
Horizontal_group::add_element(Score_elem*elt)
{
    elt->x_group_element_i_ ++;
    Elbement_group::add_element(elt);
}

void
Vertical_group::add_element(Score_elem*e)
{
    e->y_group_element_i_++;
    Elbement_group::add_element(e);
}


void
Horizontal_group::translate_x(Real x)
{
    for (int i=0; i < elem_l_arr_.size(); i++) 
	elem_l_arr_[i]->translate_x(x);
}

void
Vertical_group::translate_y(Real y)
{
    for (int i=0; i < elem_l_arr_.size(); i++) 
	elem_l_arr_[i]->translate_y(y);
}

IMPLEMENT_STATIC_NAME(Elbement_group);
IMPLEMENT_IS_TYPE_B1(Elbement_group, Score_elem);

void
Elbement_group::do_print() const
{
#ifndef NPRINT
    for (int i=0; i < elem_l_arr_.size(); i++) 
	mtor << elem_l_arr_[i]->name() << ' ';
#endif
}

void
Horizontal_group::do_substitute_dependency(Score_elem* old, Score_elem *new_l)
{
    int i;

    while ((i=elem_l_arr_.find_i(old))>=0) {
	old->x_group_element_i_--;
	
	if (new_l){ 
	    new_l->x_group_element_i_ ++;
	    elem_l_arr_[i] = new_l;
	}else {	    
	    elem_l_arr_.del(i);
	}
    }
}

void
Vertical_group::do_substitute_dependency(Score_elem* old, Score_elem *new_l)
{
    int i;

    while ((i=elem_l_arr_.find_i(old))>=0) {
	old->y_group_element_i_--;
	
	if (new_l){ 
	    new_l->y_group_element_i_ ++;
	    elem_l_arr_[i] = new_l;
	}else {	    
	    elem_l_arr_.del(i);
	}
    }
}

Vertical_group::Vertical_group(Vertical_group 
			       const &s)
    : Elbement_group(s)
{
   for (int i=0; i < elem_l_arr_.size(); i++) 
	elem_l_arr_[i]->y_group_element_i_ ++;   
}

Horizontal_group::Horizontal_group(Horizontal_group 
				   const &s)
    : Elbement_group(s)
{
   for (int i=0; i < elem_l_arr_.size(); i++) 
	elem_l_arr_[i]->x_group_element_i_ ++;   
}

Elbement_group::Elbement_group()
{
    transparent_b_ = true;
}

IMPLEMENT_IS_TYPE_B2(Horizontal_vertical_group, Horizontal_group, Vertical_group);
IMPLEMENT_STATIC_NAME(Horizontal_vertical_group);

void
Horizontal_vertical_group::do_substitute_dependency(Score_elem*o,Score_elem*n)
{
    Vertical_group::do_substitute_dependency(o,n);
    Horizontal_group::do_substitute_dependency(o,n);
}

void
Horizontal_vertical_group::add_element(Score_elem*e)
{
    Vertical_group::add_element(e);
    Horizontal_group::add_element(e);
}

void
Horizontal_vertical_group::do_print()const
{
    Vertical_group::do_print();
}

void
Vertical_group::do_print()const
{
    Elbement_group::do_print();
}

void
Horizontal_group::do_print() const
{
    Elbement_group::do_print();
}
