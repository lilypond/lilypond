/*
  note-performer.hh -- declare Note_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef NOTE_PERFORMER_HH
#define NOTE_PERFORMER_HH

#include "performer.hh"

class Note_performer : public Performer {
    Melodic_req* current_l_;
    Moment off_mom_;
    Moment on_mom_;

public:
    virtual void process_request() {
	if ( when() == off_mom_ )
 	    play_event( Note_event( current_l_->pitch() ) );
};

#endif // NOTE_PERFORMER_HH
