/*
  lyric-engraver.hh -- declare Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LYRIC_ENGRAVER_HH
#define LYRIC_ENGRAVER_HH
#include "engraver.hh"
#include "varray.hh"

#include "lily-proto.hh"

class Lyric_engraver : public Engraver {
    Lyric_req* lreq_l_;
    Text_item *lyric_item_p_;
protected:
    virtual void do_pre_move_processing();
    virtual bool do_try_request (Request*);
    virtual void do_process_requests();
    virtual void do_post_move_processing();
public:
  TRANSLATOR_CLONE(Lyric_engraver);
    DECLARE_MY_RUNTIME_TYPEINFO;
    Lyric_engraver();
};
#endif // LYRIC_ENGRAVER_HH
