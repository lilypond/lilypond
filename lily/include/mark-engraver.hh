/*
  mark-engraver.hh -- declare Mark_engraver

  source file of the GNU LilyPond music typesetter

 (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef MARK_ENGRAVER_HH
#define MARK_ENGRAVER_HH

#include "engraver.hh"

/**
  */
class Mark_engraver : public Engraver 
{
public:
  Mark_engraver ();

  TRANSLATOR_CLONE(Mark_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;

protected:
  virtual bool do_try_request (Request *req_l);
  virtual void do_process_requests ();
  virtual void do_pre_move_processing ();

private:
  Mark_req * mark_req_l_;
  Script* script_p_;
};

#endif // MARK_ENGRAVER_HH
