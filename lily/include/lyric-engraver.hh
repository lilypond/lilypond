/*
  lyric-engraver.hh -- declare Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef LYRIC_ENGRAVER_HH
#define LYRIC_ENGRAVER_HH
#include "engraver.hh"
#include "array.hh"

#include "lily-proto.hh"

class Lyric_engraver : public Engraver {
  Lyric_req* lreq_l_;
  Text_item *lyric_item_p_;
protected:
  virtual void do_pre_move_processing();
  virtual bool do_try_music (Music*);
  virtual void do_process_requests();
  virtual void do_post_move_processing();
public:
  VIRTUAL_COPY_CONS(Translator);
    
  Lyric_engraver();
};
#endif // LYRIC_ENGRAVER_HH
