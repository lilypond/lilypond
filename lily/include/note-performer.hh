/*
  note-performer.hh -- declare Note_performer

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  virtual void do_process_music ();
  virtual bool do_try_music (Music *req_l) ;
  virtual void do_print () const;
  virtual void do_pre_move_processing ();
  virtual void process_acknowledged ();
  Global_translator* global_translator_l ();

private:
  Array<Note_req*> note_req_l_arr_;
  Array<Audio_note*> note_p_arr_;
  Array<Audio_note*> delayed_p_arr_;
};

#endif // NOTE_PERFORMER_HH
