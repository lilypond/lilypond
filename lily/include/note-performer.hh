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

    virtual void process_requests();
    virtual bool /*do_*/try_request( Request *req_l ) ;

protected:
    virtual void do_print() const;

#if 0
    virtual void /*do_*/process_requests();
    virtual void /*do_*/pre_move_processing();
    virtual void /*do_*/post_move_processing();
#endif

private:
    Melodic_req * note_req_l_;
//    Rhythmic_req * note_req_l_;

//    Melodic_req* current_l_;
//    Moment off_mom_;
//    Moment on_mom_;
};

#endif // NOTE_PERFORMER_HH
