/*
  engravergroup.cc -- implement Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "engraver-group.hh"
#include "engraver.hh"
#include "debug.hh"
#include "p-score.hh"
#include "score-elem.hh"
#include "input-translator.hh"

Engraver* get_engraver_p(String);

Engraver_group_engraver::~Engraver_group_engraver()
{
    assert(removable_b());
}

void
Engraver_group_engraver::check_removal()
{
    for (int i =0; i < group_l_arr_.size(); ) {
	group_l_arr_[i]->check_removal();
	if (group_l_arr_[i]->removable_b())
	    terminate_engraver(group_l_arr_[i]);
	else 
	    i++;
    }
}

bool
Engraver_group_engraver::removable_b()const
{
    return !iterator_count_&& !group_l_arr_.size() ;
}

Engraver_group_engraver::Engraver_group_engraver()
{
    itrans_l_ = 0;
}

void
Engraver_group_engraver::set_feature(Feature d)
{
    PCursor<Engraver*> i(grav_list_.top());
    // why the while construct?
    while (i.ok()) {
	// this construction to ensure clean deletion
	Engraver *grav_l = i++; 
	grav_l->set_feature(d);
    }
}

void
Engraver_group_engraver::sync_features()
{
    PCursor<Engraver*> i(grav_list_.top());
    while (i.ok()) {
	Engraver *grav_l = i++; 
	grav_l->sync_features();
    }
}

void
Engraver_group_engraver::do_pre_move_processing()
{
    PCursor<Engraver*> i(grav_list_.top());
    while (i.ok()) {
	Engraver *grav_l = i++; 
	grav_l->pre_move_processing();
    }
}

void
Engraver_group_engraver::do_process_requests()
{
    PCursor<Engraver*> i(grav_list_.top());
    while (i.ok()) {
	Engraver *grav_l = i++; 
	grav_l->process_requests();
    }
}


void
Engraver_group_engraver::do_post_move_processing()
{
    PCursor<Engraver*> i(grav_list_.top());
    while (i.ok()) {
		// this construction to ensure clean deletion
	Engraver *grav_l = i++; 
	grav_l->post_move_processing();
    }
}


bool
Engraver_group_engraver::contains_b(Engraver* grav_l)const
{
    bool parent_b = Engraver::contains_b(grav_l);
    
    if (parent_b)
	return true;
    for (PCursor<Engraver*> i(grav_list_.top()); i.ok(); i++)
	if (i->contains_b(grav_l))
	    return true;
    return false;
}
	


bool
Engraver_group_engraver::do_try_request(Request*req_l)
{
    bool hebbes_b =false;
    for (int i =0; !hebbes_b && i < nongroup_l_arr_.size() ; i++)
	hebbes_b =nongroup_l_arr_[i]->try_request(req_l);
    if (!hebbes_b && daddy_grav_l_)
	hebbes_b = daddy_grav_l_->try_request(req_l);
    return hebbes_b ;
}

bool
Engraver_group_engraver::try_request(Request* r)
{
    return Engraver::try_request(r);
}

void
Engraver_group_engraver::add(Engraver *grav_p)
{
    grav_list_.bottom().add(grav_p);
    grav_p->daddy_grav_l_ = this;

    if (grav_p->is_type_b(Engraver_group_engraver::static_name())) {
	group_l_arr_.push((Engraver_group_engraver*)grav_p);
    } else {
	nongroup_l_arr_ .push(grav_p);
    }
}


Engraver *
Engraver_group_engraver::remove_engraver_p(Engraver*grav_l)
{
    group_l_arr_.substitute((Engraver_group_engraver*)grav_l,0);
    nongroup_l_arr_.substitute(grav_l,0);
    PCursor<Engraver*> grav_cur( grav_list_.find(grav_l) );
    
    return grav_cur.remove_p();
}

void
Engraver_group_engraver::terminate_engraver(Engraver*r_l)
{
    mtor << "Removing " << r_l->name() << " at " << get_staff_info().when() << "\n";
    r_l->do_removal_processing();
    Engraver * grav_p =remove_engraver_p(r_l);
    
    delete grav_p;
}

IMPLEMENT_IS_TYPE_B2(Engraver_group_engraver,Engraver, Translator);

ADD_THIS_ENGRAVER(Engraver_group_engraver);

void
Engraver_group_engraver::do_print()const
{
#ifndef NPRINT
    mtor << "ID: " << id_str_ << "\n";
    for (PCursor<Engraver*> i(grav_list_.top()); i.ok(); i++)
	i->print();
#endif
}


Engraver_group_engraver*
Engraver_group_engraver::find_engraver_l(String n, String id)
{
    if (name() == n && id_str_ == id)
	return this;
    Engraver_group_engraver * r = 0;
    for (int i =0; !r && i<  group_l_arr_.size(); i++) {
	r = group_l_arr_[i]->find_engraver_l(n,id);
    }
    
    return r;
}

Translator*
Engraver_group_engraver::find_get_translator_l(String n,String id)
{
    Translator * ret=0;
    Input_translator * itrans_l= itrans_l_-> recursive_find ( n );
    if (itrans_l ) {
	ret = find_engraver_l(n,id);
	if (!ret) {
	    Engraver_group_engraver * group = 
		itrans_l-> get_group_engraver_p();
	    
	    add(group);
	    ret = group;
	    
	    if (group->itrans_l_->is_name_b( n ) )
		ret ->id_str_ = id;
	    else
		return ret->find_get_translator_l(n,id);

	}
    } else if (daddy_grav_l_)
	ret =daddy_grav_l_->find_get_translator_l(n,id);
    else {
	warning("Can't find or create `" + n + "' called `" + id + "'\n");
	ret =0;
    }
    return ret;
}

int
Engraver_group_engraver::depth_i()const
{
    return daddy_grav_l_->depth_i()  + 1;
}

Translator*
Engraver_group_engraver::ancestor_l(int l)
{
    if (!l || !daddy_grav_l_)
	return this;
    
    return daddy_grav_l_->ancestor_l(l-1);
}

void
Engraver_group_engraver::announce_element(Score_elem_info info)
{
    announce_info_arr_.push(info);
    Engraver::announce_element(info);
}

void
Engraver_group_engraver::do_announces()
{
    for (int i=0; i < group_l_arr_.size(); i++) {
	group_l_arr_[i]->do_announces();
    }
    
    Request dummy_req;
 
    for (int j =0; j < announce_info_arr_.size(); j++){
       Score_elem_info info = announce_info_arr_[j];
       
       if (!info.req_l_)
	    info.req_l_ = &dummy_req;
       for (int i=0; i < nongroup_l_arr_.size(); i++) {	// Is this good enough?6
	   if (nongroup_l_arr_[i] != info.origin_grav_l_arr_[0])
	       nongroup_l_arr_[i]->acknowledge_element(info);
       }
    }
    announce_info_arr_.set_size(0);
}


void
Engraver_group_engraver::do_removal_processing()
{
    for (PCursor<Engraver*> i(grav_list_.top()); i.ok(); i++)
	i->do_removal_processing();
}

Staff_info
Engraver_group_engraver::get_staff_info()const
{
    Staff_info inf = Engraver::get_staff_info();

    for (int i=0; i < nongroup_l_arr_.size(); i++)
	nongroup_l_arr_[i]->fill_staff_info(inf);
    
    return inf;
}

Translator*
Engraver_group_engraver::get_default_interpreter()
{
    // ? 
    if ( is_bottom_engraver_b() )
	return daddy_grav_l_->get_default_interpreter();

    Engraver_group_engraver *grav_p= itrans_l_->
	get_default_itrans_l()->get_group_engraver_p();
    add(grav_p );
    if (grav_p->is_bottom_engraver_b())
	return grav_p;
    else
	return grav_p->get_default_interpreter();
}

bool
Engraver_group_engraver::is_bottom_engraver_b()const
{
    return !itrans_l_->get_default_itrans_l();
}
