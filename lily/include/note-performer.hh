/*
  note-performer.hh -- declare Note_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef NOTE_PERFORMER_HH
#define NOTE_PERFORMER_HH

#include "performer.hh"

/**
*/

class Note_performer : public Performer {
public:
    NAME_MEMBERS();

    Note_performer();
    ~Note_performer();

protected:
    virtual void process_requests();

    virtual bool do_try_request( Request *req_l ) ;
    virtual void do_print() const;

private:
    Melodic_req * note_req_l_;
    Moment off_mom_;
};

#endif // NOTE_PERFORMER_HH
