/*
  mark-engraver.hh -- declare Mark_engraver

  source file of the GNU LilyPond music typesetter

 (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef MARK_ENGRAVER_HH
#define MARK_ENGRAVER_HH

#include "engraver.hh"

class G_staff_side_item;
class G_text_item;

/**
  */
class Mark_engraver : public Engraver 
{
public:
  Mark_engraver ();

  VIRTUAL_COPY_CONS(Translator);
  

protected:
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_requests ();
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);

private:
  Mark_req * mark_req_l_;
  G_staff_side_item* staff_side_p_;
  G_text_item* text_p_;
};

#endif // MARK_ENGRAVER_HH
