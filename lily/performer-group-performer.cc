/*
  performer-group-performer.cc -- implement Performer_group_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "performer-group-performer.hh"
#include "input-translator.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B2(Performer_group_performer,Performer, Translator);
IMPLEMENT_STATIC_NAME(Performer_group_performer);
ADD_THIS_PERFORMER(Performer_group_performer);

Performer_group_performer::Performer_group_performer()
{
}

Performer_group_performer::~Performer_group_performer()
{
}

void
Performer_group_performer::add( Performer *perf_p )
{
    perf_p_list_.bottom().add(perf_p);
    perf_p->daddy_perf_l_ = this;

    if (perf_p->is_type_b(Performer_group_performer::static_name())) {
	group_l_arr_.push((Performer_group_performer*)perf_p);
    } else {
	nongroup_l_arr_ .push(perf_p);
    }
}

Translator*
Performer_group_performer::ancestor_l( int l )
{
    if (!l || !daddy_perf_l_)
	return this;
    
    return daddy_perf_l_->ancestor_l( l - 1 );
}

int
Performer_group_performer::depth_i() const
{
    return daddy_perf_l_->depth_i() + 1;
}

Translator*
Performer_group_performer::find_get_translator_l( String n,String id )
{
    Translator * ret=0;
    Input_translator* itrans_l= itrans_l_-> recursive_find ( n );
    if (itrans_l ) {
	ret = find_performer_l(n,id);
	if (!ret) {
	    Performer_group_performer * group = 
		itrans_l-> get_group_performer_p();
	    
	    add(group);
	    ret = group;
	    
	    if (group->itrans_l_->is_name_b( n ) )
		ret ->id_str_ = id;
	    else
		return ret->find_get_translator_l(n,id);

	}
    } else if (daddy_perf_l_)
	ret =daddy_perf_l_->find_get_translator_l(n,id);
    else {
	warning("Can't find or create `" + n + "' called `" + id + "'\n");
	ret =0;
    }
    return ret;
}

Performer_group_performer*
Performer_group_performer::find_performer_l( String n, String id )
{
    if (name() == n && id_str_ == id)
	return this;
    Performer_group_performer * r = 0;
    for (int i =0; !r && i<  group_l_arr_.size(); i++) {
	r = group_l_arr_[i]->find_performer_l(n,id);
    }
    
    return r;
}

Translator*
Performer_group_performer::get_default_interpreter()
{
    // ? 
    if ( is_bottom_performer_b() )
	return daddy_perf_l_->get_default_interpreter();

    Performer_group_performer *perf_p= itrans_l_->
	get_default_itrans_l()->get_group_performer_p();
    add(perf_p );
    if (perf_p->is_bottom_performer_b())
	return perf_p;
    else
	return perf_p->get_default_interpreter();
}

Moment
Performer_group_performer::get_mom() const
{
    Moment mom = Performer::get_mom();

    for ( int i = 0; i < nongroup_l_arr_.size(); i++ )
	nongroup_l_arr_[ i ]->set( mom );
    
    return mom;
}

bool
Performer_group_performer::is_bottom_performer_b() const
{
    return !itrans_l_->get_default_itrans_l();
}

void
Performer_group_performer::midi_output( Midi_stream* midi_stream_l )
{
    for ( PCursor<Performer*> i( perf_p_list_.top() ); i.ok(); i++ )
	i->midi_output( midi_stream_l );
}

void
Performer_group_performer::process_requests()
{
    for ( PCursor<Performer*> i( perf_p_list_.top() ); i.ok(); i++ )
	i->process_requests();
}

void
Performer_group_performer::set_track( Midi_def* midi_l, int& track_i_r )
{
    for ( PCursor<Performer*> i( perf_p_list_.top() ); i.ok(); i++ )
	i->set_track( midi_l, track_i_r );
}

bool
Performer_group_performer::try_request( Request* req_l )
{
//    return Performer::try_request( req_l );
    bool hebbes_b =false;
    for (int i =0; !hebbes_b && i < nongroup_l_arr_.size() ; i++)
	hebbes_b =nongroup_l_arr_[i]->try_request(req_l);
//    if (!hebbes_b)
    if ( !hebbes_b && daddy_perf_l_ )
	hebbes_b = daddy_perf_l_->try_request(req_l);
    return hebbes_b ;
}

