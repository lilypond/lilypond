/*
  note-performer.hh -- declare Note_performer

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef NOTE_PERFORMER_HH
#define NOTE_PERFORMER_HH

#include "performer.hh"

/**
*/

class Note_performer : public Performer {
public:
  TRANSLATOR_CLONE(Note_performer);
  DECLARE_MY_RUNTIME_TYPEINFO;

  Note_performer();

protected:
  virtual void do_process_requests();

  virtual bool do_try_request (Request *req_l) ;
  virtual void do_print() const;

private:
  Melodic_req * note_req_l_;
};

#endif // NOTE_PERFORMER_HH
