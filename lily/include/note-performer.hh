/*
  note-performer.hh -- declare Note_performer

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef NOTE_PERFORMER_HH
#define NOTE_PERFORMER_HH

#include "performer.hh"

/**
Convert reqs to audio notes.
*/
class Note_performer : public Performer {
public:
  VIRTUAL_COPY_CONS(Translator);
  

  Note_performer();

protected:
  virtual void do_process_requests();
  virtual bool do_try_music (Music *req_l) ;
  virtual void do_print() const;

private:
  Array<Melodic_req *> note_req_l_;
};

#endif // NOTE_PERFORMER_HH
