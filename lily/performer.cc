/*
  performer.cc -- declare Performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */


#include "performer-group-performer.hh"
#include "debug.hh"


IMPLEMENT_IS_TYPE_B(Performer);

Performer::Performer()
{
    daddy_perf_l_ = 0;
    init_b_ =false;
}

Performer::~Performer()
{
}

void
Performer::do_print() const
{
}

Moment
Performer::get_mom() const
{
    return daddy_perf_l_->get_mom();
}

void 
Performer::play_event( Midi_item* l ) 
{ 
    daddy_perf_l_->play_event( l ); 
}

int
Performer::get_tempo_i()const
{
    return daddy_perf_l_->get_tempo_i();
}

void
Performer::print() const
{
#ifndef NPRINT
    mtor << "\n" << name() << " {";
    do_print();
    mtor << "}";
#endif
}

void
Performer::process_requests()
{
}

void
Performer::set( Moment )
{
} 

bool 
Performer::do_try_request( Request* req_l )
{
    return false;
}

bool
Performer::try_request(Request*r)
{
    if (!init_b_) { 
	creation_processing();
    }
    return do_try_request(r);
}

void
Performer::creation_processing()
{
    if (!init_b_) { 
/*	if ( daddy_perf_l_ ) {
	    init_b_ = true;	// ugh. avoid recursion
	    daddy_perf_l_->creation_processing();
	    init_b_ = false;
	}
	
	*/
	do_creation_processing();
	init_b_ = true;
    }
}
void
Performer::do_creation_processing()
{
}

void
Performer::do_removal_processing()
{
}
