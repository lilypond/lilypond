/*
  performer.cc -- declare Performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */


#include "performer-group-performer.hh"
#include "debug.hh"

IMPLEMENT_STATIC_NAME(Performer);
IMPLEMENT_IS_TYPE_B(Performer);

Performer::Performer()
{
    daddy_perf_l_ = 0;
}

Performer::~Performer()
{
}

void
Performer::do_print() const
{
}

void 
Performer::play_event( Midi_item* l ) 
{ 
    daddy_perf_l_->play_event( l ); 
}

void
Performer::print() const
{
#ifndef NPRINT
    mtor << "\n" << name() << " {";
//    do_print();
    mtor << "}";
#endif
}

void
Performer::process_requests()
{
}

bool 
Performer::try_request( Request* req_l )
{
// huh?
//    return daddy_perf_l_->try_request( req_l_ );
    return false;
}

