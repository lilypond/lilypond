/*
  mark-engraver.hh -- declare Mark_engraver

  source file of the GNU LilyPond music typesetter

 (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef MARK_ENGRAVER_HH
#define MARK_ENGRAVER_HH

#include "bar-script-engraver.hh"

/**Print rehearsal marks.
  */
class Mark_engraver : public Bar_script_engraver 
{
public:
  Mark_engraver ();
  VIRTUAL_COPY_CONS(Translator);
protected:
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_requests ();
  virtual void do_post_move_processing ();
private:
  Mark_req * mark_req_l_;
};

#endif // MARK_ENGRAVER_HH
