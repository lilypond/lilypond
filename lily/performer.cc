/*
  performer.cc -- declare Performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#if 0

#include "lily-proto.hh"
#include "performer.hh"
#include "performer-group-performer.hh"

IMPLEMENT_STATIC_NAME(Performer);
IMPLEMENT_IS_TYPE_B(Performer);

Performer::Performer()
{
}

Performer::~Performer()
{
}

bool 
Performer::try_request( Request* req_l );
{
    return daddy_perf_l_->try_request( req_l );
}

void 
Performer::play_event( Midi_item i ) 
{ 
    daddy_perf_l_->play_event( i ); 
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


#endif
