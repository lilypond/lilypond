/*
  music-iterator.cc -- implement {Music,Chord,Voice}_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "music-list.hh"
#include "music-iterator.hh"
#include "translator.hh"
#include "request.hh"
#include "debug.hh"

IMPLEMENT_STATIC_NAME(Music_iterator);
IMPLEMENT_IS_TYPE_B(Music_iterator);

Chord_iterator::~Chord_iterator(){}
void
Music_iterator::do_print()const
{

}

void
Music_iterator::print() const
{
#ifndef NPRINT
    mtor << name() << "{";
    mtor << "report to " << report_to_l() << " (" << report_to_l()->name() << ")\n";
    mtor << "next at " << next_moment() << " ";
    do_print();
    mtor << "}\n";
#endif
}

Translator*
Music_iterator::get_req_translator_l()
{
    assert(report_to_l());
    if (report_to_l()->is_bottom_engraver_b() )
	return report_to_l();

    set_translator( report_to_l()->get_default_interpreter() );
    return report_to_l();
}

void
Music_iterator::push_translator(Translator*t)
{
    if (t) {
	report_to_l_arr_.push(t);
	t->iterator_count_ ++;
    }
}

void
Music_iterator::pop_translator()
{
    if (report_to_l()) {
	report_to_l()->iterator_count_ --;
	report_to_l_arr_.pop();
    }
}

Translator* 
Music_iterator::report_to_l()const
{
    if (! report_to_l_arr_.size() )
	return 0;
    return report_to_l_arr_.top();
}


void
Music_iterator::set_translator(Translator*reg)
{   
    if (report_to_l()==reg)
	return;
    pop_translator();
    push_translator(reg);
}

void
Music_iterator::construct_children()
{

}

Music_iterator::~Music_iterator()
{
    set_translator(0);
}

Moment
Music_iterator::next_moment()const
{
    return 0;
}

void
Music_iterator::process_and_next(Moment)
{
    first_b_ = false;
}

bool
Music_iterator::ok()const
{
    return first_b_;
}

Music_iterator*
Music_iterator::static_get_iterator_p(Music *m,
				      Translator *report_l)
{
    Music_iterator * p =0;
    if (m->is_type_b( Change_reg::static_name()))
	p = new Change_iterator((Change_reg*)m);
    else if (m->is_type_b( Voice_element::static_name()))
	p = new Voice_element_iterator( (Voice_element*) m);
    else if (m->is_type_b( Chord::static_name())) 
	p =  new Chord_iterator( (Chord*) m);
    else if (m->is_type_b( Voice::static_name())) 
	p =  new Voice_iterator(  (Voice*) m);
    
     if ( m->is_type_b( Music_list::static_name())) {
	Music_list* ml = (Music_list*) m;
	if (ml -> type_str_ != "") {
	    Translator * a =report_l->
		find_get_translator_l(ml-> type_str_, ml->id_str_);

		
	    p->set_translator( a);
	    
	} 
     } 
     if (! p->report_to_l() )
	 p ->set_translator(report_l);
    
    return p;
}

Music_iterator*
Music_iterator::get_iterator_p(Music*m)const
{
    Music_iterator*p = static_get_iterator_p(m,report_to_l());
    p->daddy_iter_l_ = (Music_iterator*)this;
    p->construct_children();
    return p;
}

Music_iterator::Music_iterator()
{
    daddy_iter_l_ =0;
    first_b_ = true;
}

/* ************** */

Chord_iterator::Chord_iterator(Chord const *chord_C)
{
    chord_C_ = chord_C;
}

void
Chord_iterator::construct_children()
{
    int j = 0;
    for(PCursor<Music*> i(chord_C_->music_p_list_.top());  //, int j = 0; 
    	i.ok(); j++, i++) {
	Music_iterator * mi =  get_iterator_p( i.ptr());
	set_translator(mi->report_to_l()->ancestor_l( chord_C_->multi_level_i_ ));
	if ( mi->ok() )
	    children_p_list_.bottom().add( mi );
	else 
	    delete mi;
    }
}
void
Chord_iterator::do_print() const
{
#ifndef NPRINT
    for (PCursor<Music_iterator*> i(children_p_list_.top()); i.ok(); i++) {
	i->print();
    }
#endif
}

void
Chord_iterator::process_and_next(Moment until)
{
    for (PCursor<Music_iterator*> i(children_p_list_.top()); i.ok(); ) {
	if  (i->next_moment() == until) {
	    i->process_and_next(until);
	}
	if (!i->ok()) 
	    delete i.remove_p();
	else
	    i++;
    }
    Music_iterator::process_and_next(until);

//    assert(!ok() || next_moment() > until);
}

IMPLEMENT_STATIC_NAME(Chord_iterator);
IMPLEMENT_IS_TYPE_B1(Chord_iterator,Music_iterator);

Moment
Chord_iterator::next_moment()const
{
    Moment next_ = INFTY;
    for (PCursor<Music_iterator*> i(children_p_list_.top()); i.ok(); i++)
	next_ = next_ <? i->next_moment() ;
    return next_;
}



bool
Chord_iterator::ok()const
{
    return children_p_list_.size();
}

/* ************** */

void
Voice_iterator::do_print()const
{
    if (iter_p_)
	iter_p_->print();
}

Voice_iterator::Voice_iterator(Voice const*v)
    : PCursor<Music*> ( v->music_p_list_)
{
    here_mom_ = v->offset_mom_;
    voice_C_ = v;
    iter_p_ =0;
}

void
Voice_iterator::construct_children()
{
    if (ok()) {
	iter_p_ = Music_iterator::get_iterator_p( ptr() );	
	if (iter_p_->report_to_l()->depth_i() > report_to_l()->depth_i())
	    set_translator(iter_p_->report_to_l());
    }
}

void
Voice_iterator::next_element()
{
    delete iter_p_ ;
    iter_p_ =0;
    here_mom_ += ptr()->time_int().length();
    PCursor<Music*>::next();
    construct_children();
}

Voice_iterator::~Voice_iterator()
{
    delete iter_p_;
}

IMPLEMENT_STATIC_NAME(Voice_iterator);
IMPLEMENT_IS_TYPE_B1(Voice_iterator,Music_iterator);

void
Voice_iterator::process_and_next(Moment until)
{
    while (ok()) {
	Moment local_until = until - here_mom_;
	while ( iter_p_ && iter_p_->ok() ) {
	    Moment here = iter_p_->next_moment();
	    if (here != local_until)
		return;
	    iter_p_->process_and_next(local_until);
	}
	if (!iter_p_)
	    iter_p_ = Music_iterator::get_iterator_p( ptr() );
	else if (!iter_p_->ok() )
	    next_element();
    }
    Music_iterator::process_and_next(until);
    assert(!ok() || next_moment() > until);
}

Moment
Voice_iterator::next_moment()const
{
    return iter_p_->next_moment() + here_mom_;
}

bool
Voice_iterator::ok()const
{
    return PCursor<Music*>::ok();
}

/* ***************** */


Change_iterator::Change_iterator(Change_reg * ch)
{
    change_l_ = ch;
}

IMPLEMENT_STATIC_NAME(Change_iterator);
IMPLEMENT_IS_TYPE_B1(Change_iterator,Music_iterator);

/*
  TODO: pop/pushgroup
 */
void
Change_iterator::process_and_next(Moment mom)
{
#if 0
    if ( id[0] == '-') {
	
    
    Engraver_group_engraver *group_l =
	report_to_l()->find_get_translator_l(change_l_->type_str_, 
					     change_l_->id_str_);

    report_to_l()->daddy_grav_l_->remove_engraver_p(report_to_l());
    group_l->add(report_to_l());
#endif
    Music_iterator::process_and_next(mom);
}



/* ******************** */

IMPLEMENT_STATIC_NAME(Voice_element_iterator);
IMPLEMENT_IS_TYPE_B1(Voice_element_iterator,Music_iterator);

void
Voice_element_iterator::construct_children()
{
    get_req_translator_l();
}

Voice_element_iterator::Voice_element_iterator(Voice_element*el_l)
{
    elt_l_ = el_l;
    elt_duration_ = el_l->time_int().length(); 
    last_b_ = false;
}


bool
Voice_element_iterator::ok()const
{
    return (elt_duration_ && !last_b_) || first_b_; 
}



Moment
Voice_element_iterator::next_moment()const
{
    Moment m(0);
    if  (!first_b_) 
	m = elt_duration_;
    return m;
}

void
Voice_element_iterator::do_print() const
{
#ifndef NPRINT
    mtor << "duration: " << elt_duration_;
#endif
}
void
Voice_element_iterator::process_and_next(Moment mom)
{
    if ( first_b_ ) {
	for (PCursor<Music*> i(elt_l_->music_p_list_); i.ok(); i++) {
	    assert(i->is_type_b(Request::static_name()));
	    Request * req_l = (Request*)i.ptr();
	    bool gotcha = report_to_l()->try_request(req_l);
	    if (!gotcha)
		req_l->warning("Junking request: " + String(req_l->name()));

	}
	first_b_ = false;
    }

    if ( mom >= elt_duration_ )
	last_b_ = true;  
}
