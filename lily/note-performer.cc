/*
  note-performer.cc -- implement Note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "note-performer.hh"
#include "translator.hh"
#include "input-translator.hh"
#include "musical-request.hh"
#include "audio-item.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1(Note_performer,Performer);

ADD_THIS_PERFORMER(Note_performer);

Note_performer::Note_performer()
{
    note_req_l_ = 0;
    off_mom_ = 0;
}

Note_performer::~Note_performer()
{
}

void 
Note_performer::do_print() const
{
#ifndef NPRINT
    if ( note_req_l_ ) {
    	note_req_l_->print();
	mtor << ( off_mom_ ? "on" : "off" ) << "\n";
    }
#endif
}

void 
Note_performer::process_requests() 
{
    // this is _really_ braindead, but it generates some output
    if ( !note_req_l_ || !note_req_l_->melodic()  || !note_req_l_->rhythmic() )
	return;

    // ugh! Midi specific
    Moment mom = get_mom();
    if ( !off_mom_ ) { // start note
	off_mom_ = mom + note_req_l_->duration();
	play( new Audio_note( note_req_l_, true ) );
    }
    else if ( mom == off_mom_ ) { // stop note
	play( new Audio_note( note_req_l_, false ) );
	note_req_l_ = 0;
	off_mom_ = 0;
    }
}

bool 
Note_performer::do_try_request( Request* req_l )
{
    if ( note_req_l_ )
	return false;
    
 // huh?
//    if (req_l->musical() && (req_l->musical()->note() || req_l->musical()->rest()))
//	note_req_l_ = req_l->musical()->rhythmic();
    if ( req_l->musical() && req_l->musical()->note() )
// huh?
	note_req_l_ = req_l->musical()->melodic();
    else
	return false;

    return true;
}
