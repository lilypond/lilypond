/*
  note-performer.cc -- implement Note_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "note-performer.hh"
#include "translator.hh"
#include "input-translator.hh"
#include "musical-request.hh"
#include "midi-item.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1(Note_performer,Performer);
IMPLEMENT_STATIC_NAME(Note_performer);
ADD_THIS_PERFORMER(Note_performer);

Note_performer::Note_performer()
{
    // is this what we want? 
    // or do we need to cache/keep note until it has ended?
    note_req_l_ = 0;
}

Note_performer::~Note_performer()
{
}

void 
Note_performer::do_print() const
{
#ifndef NPRINT
    if ( note_req_l_ )
    	note_req_l_->print();
#endif
}

void 
Note_performer::process_requests() 
{
//    if ( when() == off_mom_ )
//	play_event( Note_event( current_l_->pitch() ) );

//Midi_note( Melodic_req* melreq_l, int channel_i, bool on_bo  )

    // this is _really_ braindead, but it generates some output
    if ( note_req_l_ && note_req_l_->melodic() ) {
	Midi_note n( note_req_l_->melodic(), 0, true  );
	play_event( &n );
    }
    note_req_l_ = 0;
}

bool 
Note_performer::try_request( Request* req_l )
{
    if ( note_req_l_ )
	return false;
    
 // huh?
//    if (req_l->musical() && (req_l->musical()->note() || req_l->musical()->rest()))
//	note_req_l_ = req_l->musical()->rhythmic();
    if ( req_l->musical() && req_l->musical()->note() )
	note_req_l_ = req_l->musical()->melodic();
    else
	return false;

    return true;
}

